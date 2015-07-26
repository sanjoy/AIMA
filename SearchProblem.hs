module SearchProblem(SearchProblem(..)) where

data SearchProblem state = SearchProblem {
  getInitialStates :: [state],
  getSuccessorsFunc :: state -> [state],
  getIsGoalState :: state -> Bool
  }
