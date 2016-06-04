{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where

import Network.HTTP.Client
import Network.HTTP.Types

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Monoid ((<>))

import Control.Monad (liftM, mzero, forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, asks)
import Control.Monad.State (execState)
import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses, (%=), view, use, uses, _1, _2, _3, _4, _5, _6)
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

data State = State {
    stateGame    :: Game
  , stateHero    :: Hero
  , stateToken   :: Text
  , stateViewUrl :: Text
  , statePlayUrl :: Text
} deriving (Show, Eq)

instance FromJSON State where
    parseJSON (Object o) = State <$> o .: "game"
                                 <*> o .: "hero"
                                 <*> o .: "token"
                                 <*> o .: "viewUrl"
                                 <*> o .: "playUrl"
    parseJSON _ = mzero


newtype GameId = GameId Text
    deriving (Show, Eq)

instance FromJSON GameId where
    parseJSON x = GameId <$> parseJSON x

data Game = Game {
    gameId       :: GameId
  , gameTurn     :: Integer
  , gameMaxTurns :: Integer
  , gameHeroes   :: [Hero]
  , gameBoard    :: Board
  , gameFinished :: Bool
} deriving (Show, Eq)

instance FromJSON Game where
    parseJSON (Object o) = Game <$> o .: "id"
                                <*> o .: "turn"
                                <*> o .: "maxTurns"
                                <*> o .: "heroes"
                                <*> o .: "board"
                                <*> o .: "finished"
    parseJSON _ = mzero

newtype HeroId = HeroId Int
    deriving (Show, Eq)

instance FromJSON HeroId where
    parseJSON x = HeroId <$> parseJSON x

data Pos = Pos { posX :: Int , posY :: Int }
  deriving (Show, Eq, Ord)

instance FromJSON Pos where
    {-parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"-}
    {-AA 20140204 These seem to be labelled around the wrong way in the JSON-}
    parseJSON (Object o) = Pos <$> o .: "y" <*> o .: "x"
    parseJSON _ = mzero

data Tile = FreeTile | WoodTile | TavernTile | HeroTile HeroId | MineTile (Maybe HeroId)
    deriving (Show, Eq)


data Hero = Hero {
    heroId        :: HeroId
  , heroName      :: Text
  , heroUserId    :: Maybe Text
  , heroElo       :: Maybe Integer
  , heroPos       :: Pos
  , heroLife      :: Integer
  , heroGold      :: Integer
  , heroMineCount :: Integer
  , heroSpawnPos  :: Pos
  , heroCrashed   :: Bool
} deriving (Show, Eq)

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
  , _bo_tiles :: [Tile]
  , _bo_mines :: Map Pos (Maybe HeroId)
  , _bo_taverns :: Set Pos
} deriving (Show, Eq)

$(makeLenses ''Board)

instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

instance ToJSON Board where
    toJSON b  = object [ "size"  .= view bo_size b
                       , "tiles" .= (printTiles $ view bo_tiles b)
                       ]


parseBoard :: Int -> String -> Board
parseBoard s t =
  let
    chunks []       = []
    chunks (_:[])   = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs
  in
  view _2 $
  flip execState (Pos 0 0, Board s [] Map.empty Set.empty) $ do
    forM_ (chunks t) $ \t' -> do
      pos@Pos{..} <- use _1
      t <- case t' of
            (' ', ' ') -> return $ FreeTile
            ('#', '#') -> return $ WoodTile
            ('@', x)   -> return $ HeroTile $ HeroId $ read [x]
            ('[', ']') -> do
              _2 . bo_taverns %= Set.insert pos
              return TavernTile
            ('$', x)   -> do
              hid <- pure $
                case x of
                  '-' -> Nothing
                  x -> Just (HeroId $ read [x])
              _2 . bo_mines %= Map.insert pos hid
              return (MineTile hid)
            (a, b) -> error $ "parse: unknown tile pattern " ++ (show $ a:b:[])
      _1 %= const (
              if | posX == s-1 -> Pos 0 (posY+1)
                 | otherwise -> Pos (posX+1) posY)
      _2 . bo_tiles %= (t:)
    _2 . bo_tiles %= reverse


printBoard b@Board{..}
  | null _bo_tiles = ""
  | otherwise = printTiles (take _bo_size _bo_tiles) <> "\n"
             <> printBoard b{_bo_tiles = drop _bo_size _bo_tiles}

printTiles :: [Tile] -> Text
printTiles = foldl (<>) "" . map printTile

printTile FreeTile = "  "
printTile WoodTile = "##"
printTile (HeroTile (HeroId i)) = "@" <> (pack $ show i)
printTile TavernTile = "[]"
printTile (MineTile Nothing) = "$-"
printTile (MineTile (Just (HeroId i))) = "$" <> (pack $ show i)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

instance ToJSON Dir where
    toJSON Stay = String "Stay"
    toJSON North = String "North"
    toJSON South = String "South"
    toJSON East = String "East"
    toJSON West = String "West"


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
move s d = request (statePlayUrl s) (object [("dir", toJSON d)])

startArena :: (Client m) => m State
startArena = do
    url <- startUrl "arena"
    request url (object [])

startUrl :: (Client m) => Text -> m Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl




