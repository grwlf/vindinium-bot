{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where

import Network.HTTP.Client
import Network.HTTP.Types

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Hashable
import GHC.Generics (Generic)

import Control.Monad (liftM, mzero, forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, asks)
import Control.Monad.State.Strict (execState)
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.), Lens, makeLenses, (%=), view, use, uses, _1, _2, _3, _4, _5, _6)
import qualified Control.Lens as Lens
import Data.Text (Text)


newtype Key = Key Text
  deriving (Show, Eq)

instance ToJSON Key where
    toJSON (Key k) = String k

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

class (MonadIO m, MonadReader Settings m) => Client m

newtype HeroId = HeroId Int
    deriving (Show, Eq)

instance FromJSON HeroId where
    parseJSON x = HeroId <$> parseJSON x

data Pos = Pos { posX :: Int , posY :: Int }
  deriving (Show, Eq, Ord, Generic)

sqdist :: Pos -> Pos -> Int
sqdist a b = (posX a - posX b)^2 + (posY a - posY b)^2

instance Hashable Pos

instance FromJSON Pos where
    {-parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"-}
    {-AA 20140204 These seem to be labelled around the wrong way in the JSON-}
    parseJSON (Object o) = Pos <$> o .: "y" <*> o .: "x"
    parseJSON _ = mzero

data Tile = FreeTile | WoodTile | TavernTile | HeroTile HeroId | MineTile (Maybe HeroId)
    deriving (Show, Eq)


data Hero = Hero {
    _heroId        :: HeroId
  , _heroName      :: Text
  , _heroUserId    :: Maybe Text
  , _heroElo       :: Maybe Integer
  , _heroPos       :: Pos
  , _heroLife      :: Integer
  , _heroGold      :: Integer
  , _heroMineCount :: Integer
  , _heroSpawnPos  :: Pos
  , _heroCrashed   :: Bool
} deriving (Show, Eq)

$(makeLenses ''Hero)

instance FromJSON Hero where
    parseJSON (Object o) = Hero <$> o .: "id"
                                <*> o .: "name"
                                <*> o .:? "userId"
                                <*> o .:? "elo"
                                <*> o .: "pos"
                                <*> o .: "life"
                                <*> o .: "gold"
                                <*> o .: "mineCount"
                                <*> o .: "spawnPos"
                                <*> o .: "crashed"
    parseJSON _ = mzero

data Board = Board {
    _bo_size  :: Int
  , _bo_tiles :: HashMap Pos Tile
  , _bo_mines :: HashSet Pos
  , _bo_taverns :: HashSet Pos
} deriving (Show, Eq)

$(makeLenses ''Board)

boardPositions Board{..} = [ (Pos x y) | x <- [0.._bo_size-1], y <- [0.._bo_size-1]]
boardTiles b = map (view bo_tiles b HashMap.!) (boardPositions b)

boardAdjascentTiles :: (Tile -> Bool) -> Pos -> Board -> HashSet (Pos)
boardAdjascentTiles flt Pos{..} Board{..} =
  HashSet.unions $
  flip concatMap [
    (Pos (posX-1) posY),
    (Pos (posX+1) posY),
    (Pos posX (posY-1)),
    (Pos posX (posY+1))] $ \p ->
    fromMaybe [] $ do
      tile <- HashMap.lookup p _bo_tiles
      if flt tile then Just [HashSet.singleton p] else Nothing

instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

instance ToJSON Board where
    toJSON b  = object [ "size"  .= view bo_size b
                       , "tiles" .= printTiles b
                       ]

parseBoard :: Int -> String -> Board
parseBoard s t =
  let
    chunks []       = []
    chunks (_:[])   = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs
    p :: (Lens.Field1 s t a b) => Lens s t a b
    p = _1
    b :: (Lens.Field2 s t a b) => Lens s t a b
    b = _2
  in
  view b $
  flip execState (Pos 0 0, Board s HashMap.empty HashSet.empty HashSet.empty) $ do
    forM_ (chunks t) $ \ab -> do
      pos@Pos{..} <- use p
      tile <- case ab of
            (' ', ' ') -> return $ FreeTile
            ('#', '#') -> return $ WoodTile
            ('@', x)   -> return $ HeroTile $ HeroId $ read [x]
            ('[', ']') -> do
              b.bo_taverns %= HashSet.insert pos
              return TavernTile
            ('$', x)   -> do
              b.bo_mines %= HashSet.insert pos
              return (MineTile $
                case x of
                  '-' -> Nothing
                  x -> Just (HeroId $ read [x]))
            (a, b) -> error $ "parse: unknown tile pattern " ++ (show $ a:b:[])
      b.bo_tiles %= HashMap.insert pos tile
      p %= const ( if | posX == s-1 -> Pos 0 (posY+1)
                      | otherwise -> Pos (posX+1) posY )

printBoard Board{..} =
  foldl (<>) "" $
  flip map [0.._bo_size -1] $ \y ->
    foldl (<>) "\n" $
    flip map [0.._bo_size-1] $ \x ->
      printTile (_bo_tiles HashMap.! (Pos x y))

printTiles :: Board -> Text
printTiles = foldl (<>) "" . map printTile . boardTiles

printTile FreeTile = "  "
printTile WoodTile = "##"
printTile (HeroTile (HeroId i)) = "@" <> (pack $ show i)
printTile TavernTile = "[]"
printTile (MineTile Nothing) = "$-"
printTile (MineTile (Just (HeroId i))) = "$" <> (pack $ show i)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq, Ord, Generic)

instance Hashable Dir

instance ToJSON Dir where
    toJSON Stay = String "Stay"
    toJSON North = String "North"
    toJSON South = String "South"
    toJSON East = String "East"
    toJSON West = String "West"

-- | Difference between adjascent positions
posDiff :: Pos -> Pos -> Dir
posDiff p1@(Pos x1 y1) p2@(Pos x2 y2) =
  case (x2-x1, y2-y1) of
    (1,0) -> East
    (-1,0) -> West
    (0,1) -> South
    (0,-1) -> North
    _ -> error $ "assert: posDiff: non-adjascent positions: "
              ++ show p1 ++ " " ++ show p2

newtype GameId = GameId Text
    deriving (Show, Eq)

instance FromJSON GameId where
    parseJSON x = GameId <$> parseJSON x

data Game = Game {
    _gameId       :: GameId
  , _gameTurn     :: Integer
  , _gameMaxTurns :: Integer
  , _gameHeroes   :: [Hero]
  , _gameBoard    :: Board
  , _gameFinished :: Bool
} deriving (Show, Eq)

$(makeLenses ''Game)

data State = State {
    _stateGame    :: Game
  , _stateHero    :: Hero
  , _stateToken   :: Text
  , _stateViewUrl :: Text
  , _statePlayUrl :: Text
} deriving (Show, Eq)

$(makeLenses ''State)

instance FromJSON State where
    parseJSON (Object o) = State <$> o .: "game"
                                 <*> o .: "hero"
                                 <*> o .: "token"
                                 <*> o .: "viewUrl"
                                 <*> o .: "playUrl"
    parseJSON _ = mzero


instance FromJSON Game where
    parseJSON (Object o) = Game <$> o .: "id"
                                <*> o .: "turn"
                                <*> o .: "maxTurns"
                                <*> o .: "heroes"
                                <*> o .: "board"
                                <*> o .: "finished"
    parseJSON _ = mzero


request :: (Client m) => Text -> Value -> m State
request url val =
  let
    decodeBody body = either err id (eitherDecode body) where
      err e = error $ "request: unable to decode state: " ++ e

    injectKey (Object a) k =
      let
        (Object b) = object [("key", toJSON k)]
      in
        Object (a <> b)
  in do
  Settings{..} <- ask
  initReq <- liftIO $ parseUrl $ unpack url
  let req = initReq
              { method = "POST"
              , requestHeaders =
                  [ (hContentType, "application/json")
                  , (hAccept,      "application/json")
                  , (hUserAgent,   "vindinium-starter-haskell")
                  ]
              , requestBody = (RequestBodyLBS . encode) (injectKey val settingsKey)
              , responseTimeout = Nothing
              }
  liftIO $ withManager defaultManagerSettings $ \mgr ->
    liftM (decodeBody . responseBody) $ httpLbs req mgr


startTraining :: (Client m) => Maybe Int -> Maybe Board -> m State
startTraining mi mb = do
    url <- startUrl "training"
    request url (object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                       <> maybe [] (\b -> [("map",  toJSON b)]) mb))

move :: (Client m) => State -> Dir -> m State
move s d = request (s^.statePlayUrl) (object [("dir", toJSON d)])

startArena :: (Client m) => m State
startArena = do
    url <- startUrl "arena"
    request url (object [])

startUrl :: (Client m) => Text -> m Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl




