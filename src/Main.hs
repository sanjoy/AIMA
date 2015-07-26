module Main where

import MissionariesAndCannibals
import BFS

main :: IO ()
main = print $ bfs mNc
