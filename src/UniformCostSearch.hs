module UniformCostSearch(uniformCostSearch) where

import GraphSearch
import SearchProblem

import Data.Heap as H
import Data.Tuple

uniformCostStore :: (Eq a, Eq s, Show a, Show s) => FrontierStore (H.MinHeap (Node s a)) s a
uniformCostStore = FS H.empty (fmap swap . H.view) H.insert

uniformCostSearch :: (Ord s, Ord a, Show s, Show a) => SearchProblem s a -> Maybe (Node s a)
uniformCostSearch = flip graphSearch uniformCostStore
