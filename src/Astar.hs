module Astar where

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


