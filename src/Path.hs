{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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

pathLength :: Path -> Integer
pathLength Path{..} = toInteger (length p_pos) + 1


data Goal = Goal {
    _goalPath :: Path
  -- ^ Distance to the goal
  , _goalRevard :: Rational
  -- ^ Revard in 'mine' units
  , _goalHP :: Integer
  } deriving(Show)

$(makeLenses ''Goal)

goalLifePrice :: Goal -> Integer
goalLifePrice g = (pathLength (g^.goalPath)) + (g^.goalHP)

-- compareGoals :: Goal -> Goal -> Ordering
-- compareGoals g1 g2 =
--   let
--     r = (g1^.goalRevard) `compare` (g2^.goalRevard)
--     l = (pathLength $ g1^.goalPath)`compare`(pathLength $ g2^.goalPath)
--   in
--   if r == EQ then l else r


-- boardGoals :: Hero -> Board -> [Goal]
-- boardGoals h b =
--   let
--   in


heroGoals :: Hero -> Game -> [Goal]
heroGoals h g =
  let
    b = g^.gameBoard
    hp hid = (getHero g hid)^.heroLife
  in
  catMaybes $
  map (\(p,hid) -> Goal <$> (pathAstar (h^.heroPos) p b) <*> (pure $ (heroMines hid b) % 4) <*> (pure (hp hid))) $
  filter (\(_,hid) -> (h^.heroId) /= hid) $
  HashSet.toList (b^.bo_heroes)

mineGoals :: Hero -> Game -> [Goal]
mineGoals h g =
  let
    b = g^.gameBoard
  in
  catMaybes $
  map (\p -> Goal <$> (pathAstar (h^.heroPos) p b) <*> (pure 1) <*> (pure 20)) $
  filter ((/=(MineTile $ Just $ h^.heroId)) . ((b^.bo_tiles) HashMap.!)) $
  HashSet.toList (b^.bo_mines)

probeGoal :: Hero -> Game -> Maybe Goal
probeGoal h g =
  listToMaybe $
  reverse $
  sortBy (compare `on` (_goalRevard &&& ((0-).pathLength . _goalPath))) $
  filter (\go -> go^.goalRevard > 0) $
  filter (\go -> h^.heroLife > goalLifePrice go) $
  ((heroGoals h g) ++ (mineGoals h g))

nearestTaverns :: Hero -> Board -> [Pos]
nearestTaverns h b =
  map fst $
  sortBy (compare `on` snd) $
  map (id &&& (sqdist (h^.heroPos))) $
  HashSet.toList (b^.bo_taverns)

probePath :: [Pos] -> Hero -> Board -> Maybe Path
probePath ps h b =
  for ps $ \pos -> do
    case pathAstar (h^.heroPos) pos b of
      Just p -> break p
      Nothing -> return ()

-- nearestMinePath = probePath nearestMines
nearestTavernPath h b = probePath (nearestTaverns h b) h b

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

