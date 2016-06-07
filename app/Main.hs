{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding(break)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Imports
import Path
import Client as Client

newtype G a = G { unG :: ReaderT Settings IO a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadIO)

instance Client G
instance (Client m) => Client (Break r m)
instance (Client m) => Client (StateT s m)

runG :: Settings -> G a -> IO a
runG s = flip runReaderT s . unG

defaultSettings = Settings (Key "ng3ttecp") ("http://vindinium.org" :: Text)

data S = S {
    _s_client :: Client.State
  , _s_path :: Maybe Path
}

$(makeLenses ''S)

dateString :: IO String
dateString = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian (utctDay t)
  return $ printf "%d-%02d-%02d-%s" y m d (show $ utctDayTime t)


save :: (MonadIO m, Show a) => String -> a -> m ()
save nm a = liftIO $ do
  s <- dateString
  let f = "data" </> (printf "%s-%s.txt" s nm)
  writeFile (f++".tmp") (show a)
  renameFile (f++".tmp") f


main :: IO ()
main = runG defaultSettings $ do
  -- cs <- startTraining Nothing Nothing
  cs <- startArena

  flip evalStateT (S cs Nothing) $ do
    loop $ do
      cs <- use s_client
      g <- pure $ cs^.stateGame
      b <- pure $  g^.gameBoard
      me <- pure $ cs^.stateHero

      let p = me^.heroPos

      out [ view stateViewUrl cs ]
      out [ printBoard b ]

      nearTavern <- pure $
        case nearestTaverns me b of
          (p:_) | posDist p (me^.heroPos)  == 1 -> True
                | otherwise -> False
          otherwise -> False

      let diffWealth = overWealth g me

      mpath <-
        case (me^.heroLife < 21) || (nearTavern && me^.heroLife < 90) || diffWealth > 2  of
          True -> do
            return $ nearestTavernPath me g
          False -> do
            let go = probeGoal me g >>= return . view goalPath
            -- out ["Goal: ", tshow go]
            return go

      dir <- do
        case mpath of
          Just path -> do
            let (d,_) = step p path
            return d
          Nothing -> do
            out ["Empty move, life ", tshow (me^.heroLife)]
            return South

      out ["Rating ", tshow (me^.heroElo), " Wealth ", tshow diffWealth, " life ", tshow (me^.heroLife)]
      cs' <- move cs dir

      s_client %= const cs'

      when (view (stateGame.gameFinished) cs') $ do
        save "State" cs
        break ()

