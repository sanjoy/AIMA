{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}

module AStarSearch(aStarSearch) where

import GraphSearch
import SearchProblem

import Control.Monad
import Data.Heap as H
import Data.Map as M
import Data.Tuple

data HFrontier s a = HF {
  getHeap :: MinPrioHeap Integer (Node s a),
  getHeuristic :: s -> Integer,
  getCostMap :: M.Map s Integer
  }

hfPop :: HFrontier s a -> Maybe (HFrontier s a, Node s a)
hfPop (HF heap heuristic costMap) = case view heap of
  Just ((_, val), heap) -> Just (HF heap heuristic costMap, val)
  Nothing -> Nothing

hfInsert :: (Ord s) => Node s a -> HFrontier s a -> HFrontier s a
hfInsert node hf@(HF heap heuristic costMap) =
  let shouldInsert = case M.lookup (getNodeState node) costMap of
        Just cost -> cost > getNodeCost node
        Nothing -> True
  in if shouldInsert then
       hf { getHeap = H.insert (heuristic (getNodeState node) + (getNodeCost node), node) heap,
            getCostMap = M.insert (getNodeState node) (getNodeCost node) costMap
          }
     else
       hf

aStarFS :: (Eq a, Eq s, Ord s, Show a, Show s) => (s -> Integer) -> FrontierStore (HFrontier s a) s a
aStarFS heuristic = FS (HF H.empty heuristic M.empty) hfPop hfInsert

aStarSearch :: (Ord s, Ord a, Show s, Show a) => InformedSearchProblem s a -> Maybe (Node s a)
aStarSearch searchProblem =
  flip graphSearch (aStarFS $ getSearchHeuristic searchProblem) $ getBaseProblem searchProblem
