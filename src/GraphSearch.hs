{-# LANGUAGE RankNTypes #-}

module GraphSearch(graphSearch, Node(..), FrontierStore(..), prettyShowSolution) where

import qualified Data.Set as S
import qualified Data.List as L

import SearchProblem

data Node state =
    Node { getNodeCost :: Integer, getNodeParent :: Node state, getNodeState :: state }
  | NodeRoot deriving(Show)

instance Eq a => Eq (Node a) where
  (Node c p s) == (Node c' p' s') = c == c' && p == p' && s == s'

instance Eq a => Ord (Node a) where
  compare x y = compare (getNodeCost x) (getNodeCost y)

data FrontierStore f v =
  FS { getEmptyStore :: f, getPop :: Ord f => f -> Maybe (f, Node v), getInsert :: Node v -> f -> f }

prettyShowSolution :: (Show a) => Maybe (Node a) -> String
prettyShowSolution Nothing = "No Solution"
prettyShowSolution (Just n) =
  let cost = getNodeCost n
      states = L.unfoldr unfoldNode n
      unfoldNode n = case n of
        NodeRoot -> Nothing
        _ -> Just (getNodeState n, getNodeParent n)
  in "Cost = " ++ show cost ++ ": " ++ (L.intercalate "; " $ map show states)

graphSearch :: (Ord v, Ord f, Show v, Show f) => SearchProblem v act -> FrontierStore f v -> Maybe (Node v)
graphSearch ssp store =
  let frontierInsert = getInsert store
      frontierEmpty = getEmptyStore store
      frontierPop = getPop store

      goalPred = getGoalPredicate ssp
      genActs = getGenApplicableActions ssp

      apply = getApplyAction ssp
      initialFrontier = frontierInsert (Node 0 NodeRoot (getInitialState ssp)) frontierEmpty

      expandChildren node =
        let actsAndCosts = genActs (getNodeState node)
            mapFst f (a,b) = (f a, b)
            statesAndCosts = map (mapFst (apply $ getNodeState node)) actsAndCosts
        in map (\(st, cost) -> Node (cost + getNodeCost node) node st) statesAndCosts

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
