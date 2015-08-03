{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}

module AStarSearch(aStarSearch) where

import GraphSearch
import SearchProblem

import Data.Heap as H
import Data.Tuple

data HeuristicStoreNode s a = HeuristicStoreNode {
  getHSNValue :: Node s a, getHSNHeuristicCost :: Integer
  } deriving(Eq, Show)

instance (Eq s, Eq a) => Ord (HeuristicStoreNode s a) where
  compare (HeuristicStoreNode _ f) (HeuristicStoreNode _ f') = compare f f'

bestFirstStore :: (Eq a, Eq s, Show a, Show s) =>
                  (s -> Integer) -> FrontierStore (H.MinHeap (HeuristicStoreNode  s a)) s a
bestFirstStore heuristic = FS H.empty (fmap ((fmap getHSNValue) . swap) . H.view) (H.insert . makeHSN)
  where makeHSN n = HeuristicStoreNode n $ getNodeCost n + (heuristic $ getNodeState n)

aStarSearch :: (Ord s, Ord a, Show s, Show a) => InformedSearchProblem s a -> Maybe (Node s a)
aStarSearch searchProblem =
  flip graphSearch (bestFirstStore $ getSearchHeuristic searchProblem) $ getBaseProblem searchProblem
