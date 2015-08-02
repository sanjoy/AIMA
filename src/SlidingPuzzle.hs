{-# LANGUAGE TupleSections #-}

module SlidingPuzzle(slidingPuzzle) where

import Data.Array
import Data.List.Ordered
import Debug.Trace

import SearchProblem

data SlidingState = SlidingState { getArray :: Array (Int, Int) Int, getBlankPos :: (Int, Int) }
                    deriving(Show, Eq, Ord)

type SlidingAction = (Int, Int)

addTuple (x, y) (p, q) = (x + p, y + q)

actions :: SlidingState -> [(SlidingAction, Integer)]
actions (SlidingState array blankPos) = map (,1) $ filter isLegal possibleMoves
  where isLegal :: (Int, Int) -> Bool
        isLegal move = (inRange (bounds array) $ addTuple move blankPos)

        possibleMoves = [(1, 0), (-1, 0), (0, 1), (0, -1)]

apply :: SlidingState -> SlidingAction -> SlidingState
apply (SlidingState array blankPos) action =
  let newBlankPos = addTuple blankPos action
      newArray = array // [(blankPos, array ! newBlankPos), (newBlankPos, -1)]
  in SlidingState newArray newBlankPos

isGoal :: SlidingState -> Bool
isGoal (SlidingState array blankPos) = isSorted $ elems array

indexHelper :: Int -> [(Int, Int)]
indexHelper size = map (\i->(i `div` size, i `rem` size)) [0 .. (size * size) - 1]

eightPuzzle :: SlidingState
eightPuzzle = SlidingState ar (2, 1)
  where ar = array ((0, 0), (2,2)) $ zip (indexHelper 3) [4,3,8,1,6,7,2,-1,5]

slidingPuzzle :: SearchProblem SlidingState SlidingAction
slidingPuzzle = SearchProblem eightPuzzle actions apply isGoal
