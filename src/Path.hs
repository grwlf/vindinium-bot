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
  -- , _goalHP :: Integer
  , _goalHP :: Integer
  , _goalPathFactor :: Integer
  } deriving(Show)

$(makeLenses ''Goal)

goalLifePrice :: Goal -> Integer
goalLifePrice g = ((g^.goalPathFactor) * pathLength (g^.goalPath)) + (g^.goalHP)

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
    nearTavern hid =
      let
        hp = (getHero g hid)^.heroPos
        tavs = b^.bo_taverns
      in
      any ((==1) . (posDist hp)) tavs

  in
  catMaybes $
  map (\(p,hid) ->
    Goal
      <$> (pathAstar (h^.heroPos) p g h)
      <*> (pure $ (heroMines hid b) % 4)
      <*> (pure (hp hid))
      <*> (pure 2)) $
  filter (\(_,hid) -> not $ nearTavern hid) $
  filter (\(_,hid) -> (h^.heroId) /= hid) $
  HashSet.toList (b^.bo_heroes)

mineGoals :: Hero -> Game -> [Goal]
mineGoals h g =
  let
    b = g^.gameBoard
  in
  catMaybes $
  map (\p -> Goal
    <$> (pathAstar (h^.heroPos) p g h)
    <*> pure 1
    <*> pure 20
    <*> pure 1) $
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

probePath :: [Pos] -> Hero -> Game -> Maybe Path
probePath ps h g =
  let
    b = g^.gameBoard
  in
  for ps $ \pos -> do
    case pathAstar (h^.heroPos) pos g h of
      Just p -> break p
      Nothing -> return ()

-- nearestMinePath = probePath nearestMines
nearestTavernPath h g = probePath (nearestTaverns h (g^.gameBoard)) h g

pathAstar :: Pos -> Pos -> Game -> Hero -> Maybe Path
pathAstar from to g me =
  let
    b = g^.gameBoard

    near p =
      let
        tiles =
          boardAdjascentTiles (
            \case
              FreeTile -> True
              HeroTile _ -> True
              _ -> False) p b

        safe = any (
          \pos ->
            case (b^.bo_tiles) HashMap.! pos of
              TavernTile -> True
              _ -> False
          ) tiles

        danger = any (
          \pos ->
            case (b^.bo_tiles) HashMap.! pos of
              HeroTile hid -> ((getHero g hid)^.heroLife) > (me^.heroLife)
              _ -> False
          ) tiles
      in
      case safe || not danger of
        True -> tiles
        False -> HashSet.empty

    dist1 _ _ = 1

    goals = HashSet.toList (near to)

    heu p = case map (\p' -> sqdist p p') goals of
              x@(_:_) -> minimum x
              _ -> 0

    isgoal p = any (\p' -> p == p') goals

  in
  Path to <$>
  aStar near dist1 heu isgoal from


-- | Paths between mines
-- minePaths :: Board -> HashMap (Pos,Pos) Path
-- minePaths b@Board{..} =
--   let
--     mines = [(m1,m2) | m1 <- HashSet.toList _bo_mines, m2 <- HashSet.toList _bo_mines, m1 /= m2 ]
--   in
--   flip execState HashMap.empty $ do
--   forM_ mines $ \(m1,m2) -> do
--     case pathAstar m1 m2 b of
--       Just p -> modify (HashMap.insert (m1,m2) p)
--       Nothing -> return ()

