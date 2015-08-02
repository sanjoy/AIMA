module Main where

import MissionariesAndCannibals
import UCS

main :: IO ()
main = print $ uniformCostSearch missionariesAndCannibals
