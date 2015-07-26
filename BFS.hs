{-# LANGUAGE TupleSections #-}

module BFS(bfs) where

import SearchProblem

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Mby
import qualified Data.Set as S


import Debug.Trace

import Control.Monad.State

type BFSState s a = StateT (S.Set s) Maybe a

bfs :: (Ord s, Show s) => SearchProblem s -> Maybe [(s, s)]
bfs prb =
  let mResult =
        bfsInner (getSuccessorsFunc prb) (getIsGoalState prb) (getInitialStates prb)
  in fst $ runState mResult S.empty
  where
    bfsInner :: (Ord s, Show s) => (s -> [s]) -> (s -> Bool) -> [s] -> State (S.Set s) (Maybe [(s, s)])
    bfsInner gen isGoal states =
      let newStates = concatMap (\s -> fmap (s,) $ gen s) states
      in do
        visited <- get
        let newUnvisitedStates = filter (\(from, to) -> to `S.notMember` visited) newStates
        case L.find (isGoal . snd) newUnvisitedStates of
          Just v -> return $ Just [v]
          Nothing ->
            if newUnvisitedStates == [] then return $ Nothing
            else do
              put (S.fromList (map snd newUnvisitedStates) `S.union` visited)
              recurse <- bfsInner gen isGoal (fmap snd newUnvisitedStates)
              case recurse of
                Nothing -> return Nothing
                Just ((from, to):rest) ->
                  let parent = Mby.fromJust (L.find (\(_, t) -> t == from) newUnvisitedStates)
                  in return $ Just $ parent:(from, to):rest
