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

mineAC _ = AttackCost 20
heroAC h = AttackCost (h^.heroLife + 20)
tavernAC _ = AttackCost 0

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

      mpath <-
        case (me^.heroLife < 21) || (nearTavern && me^.heroLife < 90)  of
          True -> do
            return $ nearestTavernPath me b
          False -> do
            let go = probeGoal me g >>= return . view goalPath
            -- out ["Goal: ", tshow go]
            return go

      dir <- do
        case mpath of
          Just path -> do
            let (d,_) = step p path
            out ["Moving towards ", tshow (p_final path), " by issuing ", tshow d, " life ", tshow (me^.heroLife)]
            return d
          Nothing -> do
            out ["Empty move, life ", tshow (me^.heroLife)]
            return South

      cs' <- move cs dir

      s_client %= const cs'

      when (view (stateGame.gameFinished) cs') $ do
        break ()

