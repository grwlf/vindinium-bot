{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (break)
import Data.Text (unpack, Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Client
import Control.Break
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT, execStateT, put, get)
import Control.Lens (makeLenses, (%=), view, use, uses, _1, _2, _3, _4, _5, _6)


newtype G a = G { unG :: ReaderT Settings IO a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadIO)

instance Client G
instance (Client m) => Client (Break r m)
instance (Client m) => Client (StateT s m)

runG :: Settings -> G a -> IO a
runG s = flip runReaderT s . unG

defaultSettings = Settings (Key "ng3ttecp") ("http://vindinium.org" :: Text)

bot :: (Client m) => State -> m Dir
bot s = return South

out :: (MonadIO m) => [Text] -> m ()
out = liftIO . Text.putStrLn . Text.concat

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

main :: IO ()
main = runG defaultSettings $ do
  s <- startTraining Nothing Nothing
  out [ stateViewUrl s ]

  flip evalStateT s $ do
    loop $ do
      s <- get
      let b = gameBoard $ stateGame s
      out [ printBoard b ]
      out [ tshow $ view bo_mines b]
      s' <- bot s >>= move s
      put s'
      when (gameFinished $ stateGame $ s') $ do
        break ()

