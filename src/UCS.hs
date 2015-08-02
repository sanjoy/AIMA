module UCS(uniformCostSearch) where

import GraphSearch
import SearchProblem

import Data.Heap as H
import Data.Tuple

uniformCostStore :: (Eq a, Show a) => FrontierStore (H.MinHeap (Node a)) a
uniformCostStore = FS H.empty (fmap swap . H.view) H.insert

uniformCostSearch :: (Ord v, Show v) => SearchProblem v act -> Maybe (Node v)
uniformCostSearch = flip graphSearch uniformCostStore
