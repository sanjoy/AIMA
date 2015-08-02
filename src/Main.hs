{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import MissionariesAndCannibals
import SlidingPuzzle
import UCS

main :: IO ()
main = do
  print $ uniformCostSearch missionariesAndCannibals
  print $ uniformCostSearch slidingPuzzle
