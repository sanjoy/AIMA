{-# LANGUAGE RankNTypes #-}
module GraphSearch(graphSearch, Node(..), FrontierStore(..)) where

import qualified Data.Set as S
import SearchProblem

--import Debug.Trace

data Node state =
    Node { getNodeCost :: Integer, getNodeParent :: Node state, getState :: state }
  | NodeRoot deriving(Show)

instance Eq a => Eq (Node a) where
  (Node c p s) == (Node c' p' s') = c == c' && p == p' && s == s'

instance Eq a => Ord (Node a) where
  compare x y = compare (getNodeCost x) (getNodeCost y)

data FrontierStore f v =
  FS { getEmptyStore :: f, getPop :: Ord f => f -> Maybe (f, Node v), getInsert :: (Node v) -> f -> f }

graphSearch :: (Ord v, Ord f, Show v, Show f) => SearchProblem v act -> FrontierStore f v -> Maybe (Node v)
graphSearch ssp store =
  let frontierInsert = getInsert store
      frontierEmpty = getEmptyStore store
      frontierPop = getPop store

      goalPred = getGoalPredicate ssp
      genActs = getGenApplicableActions ssp

      apply = getApplyAction ssp
      initialFrontier = frontierInsert (Node 0 NodeRoot (getInitialState ssp)) frontierEmpty

      expandChildren node
--        | trace ("expandChildren: node = " ++ show node) False = undefined
        | otherwise =
          let actsAndCosts = genActs (getState node)
              mapFst f (a,b) = (f a, b)
              statesAndCosts = map (mapFst (apply $ getState node)) actsAndCosts
          in map (\(st, cost) -> Node (cost + getNodeCost node) node st) statesAndCosts

      searchIter frontier explored
--        | trace ("search: state = " ++ show (frontier, explored)) False = undefined
        | otherwise = frontierPop frontier >>= searchIterHelper explored

      searchIterHelper explored (frontier, bestNode)
--        | trace ("helper: state = " ++ show (frontier, bestNode, explored)) False = undefined
        | otherwise =
           if goalPred (getState bestNode) then
             Just bestNode
           else
             let children = expandChildren bestNode
                 explored' = S.insert (getState bestNode) explored
                 newChildren = filter (\c -> (getState c) `S.notMember` explored') children
                 frontier' = foldl (flip frontierInsert) frontier newChildren
             in searchIter frontier' explored'

  in searchIter initialFrontier S.empty
