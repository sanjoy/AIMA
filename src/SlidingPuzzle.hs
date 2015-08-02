{-# LANGUAGE TupleSections #-}

module SlidingPuzzle(slidingPuzzle8, slidingPuzzle24) where

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

twentyFourPuzzle :: SlidingState
twentyFourPuzzle = SlidingState ar (2, 1)
  where ar = array ((0, 0), (4, 4)) $ zip (indexHelper 5) arList
        arList = [ 6, 14,  5, 12, 15,
                   1,  8,  7, 16,  9,
                   2, -1,  3,  4, 22,
                  11, 17, 13, 20, 24,
                  21, 10, 23, 18, 19 ]

slidingPuzzle8 :: SearchProblem SlidingState SlidingAction
slidingPuzzle8 = SearchProblem eightPuzzle actions apply isGoal

slidingPuzzle24 :: SearchProblem SlidingState SlidingAction
slidingPuzzle24 = SearchProblem twentyFourPuzzle actions apply isGoal
