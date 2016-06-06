{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Path where

import Prelude hiding(break)
import Data.Graph.AStar
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

import Imports
import Client

data Path = Path { p_final :: Pos, p_pos :: [Pos]  }
  deriving(Show,Eq,Ord)


step :: Pos -> Path -> (Dir, Maybe Path)
step p Path{..} =
  case p_pos of
    (p':ps) -> (posDiff p p', Just $ Path{..} {p_pos = ps})
    [] -> (posDiff p p_final, Nothing)

-- | Paths between mines
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

-- | Returns list of (mine,distance), not owned by the Hero, smaller distances first
nearestMines :: Hero -> Board -> [(Pos,Int)]
nearestMines h b =
  sortBy (compare `on` snd) $
  map (id &&& (sqdist (h^.heroPos))) $
  filter ((/=(MineTile $ Just $ h^.heroId)) . ((b^.bo_tiles) HashMap.!)) $
  HashSet.toList (b^.bo_mines)

nearestMinePath :: Hero -> Board -> Maybe (Pos, Path)
nearestMinePath h b =
  for (nearestMines h b) $ \(m,_) ->
    case pathAstar (h^.heroPos) m b of
      Just p -> break (m,p)
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

    goals = HashSet.toList (near to)

    heu p = minimum $ map (\p' -> sqdist p p') goals

    isgoal p = any (\p' -> p == p') goals

  in
  Path to <$>
  aStar near dist1 heu isgoal from

