{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables #-}
module BFS where

import Data.Heap (MaxPrioHeap)
import qualified Data.Heap as Heap

import Data.List
import qualified Data.List as List

import Data.Set(Set)
import qualified Data.Set as Set

type Score = Int -- Precise score, for selecting the best solution
type EstScore = Int -- Estimated score, for guiding the search
type Hash = Int

-- | Walks the search space, generates all possible moves in best first order
bestFirstSearch :: forall s fp. (Ord fp) => (s -> fp) -> (s -> [s]) -> (s -> EstScore) -> s -> [(EstScore, s)]
bestFirstSearch fingerprinter move_generator evaluator start_state =
  bfs Set.empty init where

    init :: MaxPrioHeap EstScore s
    init = Heap.fromAscList [(evaluator start_state, start_state)]

    bfs :: Set fp -> MaxPrioHeap EstScore s -> [(EstScore, s)]
    bfs visited_states solutions =
      case Heap.view solutions of
        Nothing -> []
        Just (best, rest) ->
          let
            cur_st = snd best
            finger = fingerprinter cur_st
            visited_states' = Set.insert finger visited_states
            child_states = move_generator cur_st
            child_scores = map evaluator child_states
            solutions' = foldl' (flip Heap.insert) rest $ zip child_scores child_states
          in
          case Set.notMember finger visited_states of
            {- output new state and take children into account -}
            True -> best : bfs visited_states' solutions'
            {- discarding computation, make use of laziness -}
            False -> bfs visited_states rest

