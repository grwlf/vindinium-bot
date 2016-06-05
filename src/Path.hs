{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Path where

import Data.Graph.AStar
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

import Imports
import Client

data Path = Path { p_pos :: [Pos] }
  deriving(Show,Eq,Ord)


minePaths :: Board -> HashMap (Pos,Pos) Path
minePaths b@Board{..} =
  let
    mines = [(m1,m2) | m1 <- HashSet.toList _bo_mines, m2 <- HashSet.toList _bo_mines, m1 /= m2 ]
  in
  flip execState HashMap.empty $ do
  forM_ mines $ \(m1,m2) -> do
    case pathAstar m1 m2 b of
      Just p -> modify (HashMap.insert (m1,m2) p)
      Nothing -> return ()

pathAstar :: Pos -> Pos -> Board -> Maybe Path
pathAstar from to b =
  let
    near p =
      boardAdjascentTiles (
        \case
          FreeTile -> True
          HeroTile _ -> True
          _ -> False) p b

    dist1 _ _ = 1

    heu p = minimum $ map (sqdist p) goals

    goals = HashSet.toList (near to)

    isgoal x = any (==x) goals
  in
  Path <$>
  aStar near dist1 heu isgoal from
