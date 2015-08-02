{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs.Implicit
import System.Exit

import MissionariesAndCannibals
import SlidingPuzzle
import UCS

data ProblemKind = NonePK | MissionariesAndCannibalsPK | SlidingPuzzlePK deriving(Show, Eq, Data)
data SolverKind = NoneSK | UniformCostSearchSK deriving(Show, Eq, Data)

data Arguments = Arguments { problem :: ProblemKind, solver :: SolverKind } deriving(Show, Eq, Data)

arguments = Arguments { problem = NonePK &= help "Missionaries and Cannibals",
                        solver = NoneSK &= help "Uniform cost search" }

main :: IO ()
main = do
  (Arguments problemKind solverKind) <- cmdArgs arguments
  getProblem <- case problemKind of
    NonePK -> do
      putStrLn "please specify a problem type using --problem"
      exitWith $ ExitFailure 1
      return undefined
    MissionariesAndCannibalsPK -> return missionariesAndCannibals
    SlidingPuzzlePK -> return slidingPuzzle
  getSolver <- case solverKind of
    NoneSK -> do
      putStrLn "please specify a solver type using --solver"
      exitWith $ ExitFailure 1
      return undefined
    UniformCostSearchSK -> return uniformCostSearch
  print $ getSolver getProblem
