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

bot :: (Client m, MonadState S m) => m Dir
bot = return South

main :: IO ()
main = runG defaultSettings $ do
  cs <- startTraining Nothing Nothing
  out [ view stateViewUrl cs ]

  flip evalStateT (S cs Nothing) $ do
    loop $ do
      cs <- use s_client
      b <- use (s_client.stateGame.gameBoard)
      me <- pure (cs^.stateHero)

      let p = me^.heroPos

      out [ printBoard b ]

      mp <- pure $
        case me^.heroLife > 20 of
          True -> nearestMinePath me b
          False -> nearestTavernPath me b

      dir <- do
        case mp of
          Just (goal,path) -> do
            let (d,_) = step p path
            out ["Moving towards ", tshow goal, " bu issuing ", tshow d]
            return d
          Nothing -> do
            return South

      cs' <- move cs dir

      s_client %= const cs'

      when (view (stateGame.gameFinished) cs') $ do
        break ()

