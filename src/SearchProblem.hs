module SearchProblem(SearchProblem(..), InformedSearchProblem(..)) where

-- Specification of a problem that can be solved by a graph search.

data SearchProblem state action = SearchProblem {
  getInitialState :: state,
  getGenApplicableActions :: state -> [(action, Integer)],
  getApplyAction :: state -> action -> state,
  getGoalPredicate :: state -> Bool
  }


-- A search problem with a heuristic

data InformedSearchProblem state action = InformedSearchProblem {
  baseProblem :: SearchProblem state action,
  heuristic :: state -> Integer
  }
