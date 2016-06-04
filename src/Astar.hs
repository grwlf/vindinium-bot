module Lib.Astar where

import Lib.Types
import Lib.Board
import Lib.BoardQuery

import Control.Monad

import Data.Maybe
import Data.Foldable (foldl', foldl)
import Data.Graph.AStar

import Data.List hiding (foldl', foldl)
import qualified Data.List as List

import Data.Set hiding (map, foldl, foldl')
import qualified Data.Set as Set

import Data.Map hiding (map, foldl, foldl')
import qualified Data.Map as Map

import Prelude hiding (map, foldl, foldl')
import qualified Prelude as Prelude

-- | Search the path from GameUnit @u to Cell @goal, using Board @b
-- FIXME: POORLY TESTED
pathAstar :: Cell -> Board -> Cell -> Maybe [Cell]
pathAstar from b goal =
    aStar mynei mydist heu mygoal from where
        mynei = (aStarNeighbours b)
        mydist a b = cellDist a b
        heu c' = cellDist from c'
        mygoal = (==goal)
     
-- | Search the path from GameUnit @u to Cell @goal, using Board @b
-- FIXME: POORLY TESTED
pathUnitAstar :: GameUnit -> Board -> GameUnit -> Maybe [GameUnit]
pathUnitAstar from b goal =
    aStar mynei mydist heu mygoal from where
        mynei = (aStarUnitNeighbours b)
        mydist a b = cellDist (gu_pivot a) (gu_pivot b)
        heu c' = cellDist (gu_pivot from) (gu_pivot c')
        mygoal = (== unitLowerPoint goal).unitLowerPoint

-- Moves unit from @start to @stop position over the board b
-- FIXME: UNTESTED
-- commandAstar :: GameUnit -> Board -> GameUnit -> Maybe [[Command]]
-- commandAstar start b stop =
--     aStar mynei mydist heu mygoal ([] :: [Command]) where

--         h = board_height b

--         go u cs = go' u (reverse cs)
--         go' u [] = u
--         go' u (c:cs) = go (moveUnit (command_code c) u) cs

--         mynei :: [Command] -> Set [Command]
--         mynei cs = Set.fromList (map (:cs) allCommands)

--         mydist a b = 1

--         heu :: [Command] -> Int
--         heu cs = h - (cell_y $ unitUpperPoint (go start cs))

--         mygoal cs = (go start cs) == stop


