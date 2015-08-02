{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GraphSearch
import MissionariesAndCannibals
import SlidingPuzzle
import UCS

main :: IO ()
main = do
  putStrLn "Missionaries & Cannibals, uniform cost search: "
  putStrLn $ "  " ++ (prettyShowSolution $ uniformCostSearch missionariesAndCannibals)
  putStrLn ""

  putStrLn "Sliding Puzzle, uniform cost search: "
  putStrLn $ "  " ++ (prettyShowSolution $ uniformCostSearch slidingPuzzle)
  putStrLn ""
