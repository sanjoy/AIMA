{-# LANGUAGE RankNTypes #-}

module GraphSearch(graphSearch, Node(..), FrontierStore(..), prettyShowSolution) where

import qualified Data.Set as S
import qualified Data.List as L

import SearchProblem

data Node state action =
    Node { getNodeCost :: Integer, getNodeParent :: Node state action, getNodeState :: state,
           getNodeAction :: action }
  | NodeRoot { getNodeState :: state, getNodeCost :: Integer } deriving(Show)

instance (Eq s, Eq a) => Eq (Node s a) where
  (Node c p s a) == (Node c' p' s' a') = c == c' && p == p' && s == s' && a == a'

instance (Eq s, Eq a) => Ord (Node s a) where
  compare x y = compare (getNodeCost x) (getNodeCost y)

data FrontierStore f s a =
  FS { getEmptyStore :: f, getPop :: f -> Maybe (f, Node s a), getInsert :: (Node s a) -> f -> f }

prettyShowSolution :: (Show a) => Maybe (Node s a) -> String
prettyShowSolution Nothing = "No Solution"
prettyShowSolution (Just n) =
  let cost = getNodeCost n
      actions = reverse $ L.unfoldr unfoldNode n
      unfoldNode n = case n of
        NodeRoot _ _ -> Nothing
        _ -> Just (getNodeAction n, getNodeParent n)
  in "Cost = " ++ show cost ++ ": " ++ (L.intercalate "; " $ map show actions)

graphSearch :: (Ord s, Ord a, Show s, Show a) =>
               SearchProblem s a -> FrontierStore f s a -> Maybe (Node s a)
graphSearch ssp store =
  let frontierInsert = getInsert store
      frontierEmpty = getEmptyStore store
      frontierPop = getPop store

      goalPred = getGoalPredicate ssp
      genActs = getGenApplicableActions ssp

      apply = getApplyAction ssp
      initialFrontier = frontierInsert (NodeRoot (getInitialState ssp) 0) frontierEmpty

      expandChildren node =
        let actsAndCosts = genActs (getNodeState node)
            getStateFromAction (a, c) = (apply (getNodeState node) a, a, c)
            statesActionsAndCosts = map getStateFromAction actsAndCosts
        in map (\(st, act, cost) -> Node (cost + getNodeCost node) node st act) statesActionsAndCosts

      searchIter frontier explored = frontierPop frontier >>= searchIterHelper explored

      searchIterHelper explored (frontier, bestNode) =
        if goalPred (getNodeState bestNode) then
          Just bestNode
        else
          let children = expandChildren bestNode
              explored' = S.insert (getNodeState bestNode) explored
              newChildren = filter (\c -> getNodeState c `S.notMember` explored') children
              frontier' = foldl (flip frontierInsert) frontier newChildren
          in searchIter frontier' explored'

  in searchIter initialFrontier S.empty
