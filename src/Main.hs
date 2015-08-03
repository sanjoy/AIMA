{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad

import GraphSearch
import AStarSearch
import MissionariesAndCannibals
import SlidingPuzzle
import UniformCostSearch

main :: IO ()
main = do
  putStrLn "Missionaries & Cannibals, uniform cost search: "
  putStrLn $ "  " ++ (prettyShowSolution $ uniformCostSearch missionariesAndCannibals)
  putStrLn ""

  putStrLn "Sliding Puzzle, 8 blocks, uniform cost search: "
  putStrLn $ "  " ++ (prettyShowSolution $ uniformCostSearch slidingPuzzle8)
  putStrLn ""

  when False $ do -- Uniform cost search is too slow for slidingPuzzle24
    putStrLn "Sliding Puzzle, 24 blocks, uniform cost search: "
    putStrLn $ "  " ++ (prettyShowSolution $ uniformCostSearch slidingPuzzle24)
    putStrLn ""

  putStrLn "Sliding Puzzle, 8 blocks, greedy best first search: "
  putStrLn $ "  " ++ (prettyShowSolution $ aStarSearch $ informedSlidingPuzzle slidingPuzzle8)
  putStrLn ""

  putStrLn "Sliding Puzzle, 24 blocks, greedy best first search: "
  putStrLn $ "  " ++ (prettyShowSolution $ aStarSearch $ informedSlidingPuzzle slidingPuzzle24)
  putStrLn ""
