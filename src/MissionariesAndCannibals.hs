{-# LANGUAGE TupleSections #-}
module MissionariesAndCannibals(missionariesAndCannibals) where

import SearchProblem
import UCS

data MCState = MCState { getStateM :: Int, getStateC :: Int, getStateBoat :: Bool } deriving (Show, Eq, Ord)
data Action = Action { getActionM :: Int, getActionC :: Int } deriving(Show, Eq, Ord)

isGoal :: MCState -> Bool
isGoal (MCState 0 0 _) = True
isGoal _ = False

otherSide (MCState m c b) = MCState (3 - m) (3 - c) b

isValid :: MCState -> Bool
isValid s = (getStateM s >= getStateC s) || getStateM s == 0

actions :: MCState -> [(Action, Integer)]
actions state = map (, 1) actionsInner
  where actionsInner
          | isValid state && isValid (otherSide state) =
              [Action 2 0, Action 1 0, Action 1 1, Action 0 1, Action 0 2]
          | otherwise = []

apply :: MCState -> Action -> MCState
apply state act =
  let sign = if getStateBoat state then -1 else 1
      newM = getStateM state + sign * getActionM act
      newC = getStateC state + sign * getActionC act
      newB = not $ getStateBoat state
  in MCState newM newC newB

missionariesAndCannibals :: SearchProblem MCState Action
missionariesAndCannibals = SearchProblem initialState actions apply isGoal
  where initialState = MCState 3 3 True
