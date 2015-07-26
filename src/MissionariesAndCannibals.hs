{-# LANGUAGE TupleSections #-}
module MissionariesAndCannibals(mNc) where

import BFS
import SearchProblem

type M = Int
type C = Int
newtype Boat = Boat (Bool, M, C) deriving (Eq, Show, Ord)
newtype Shore = Shore (M, C) deriving (Eq, Show, Ord)

data MCState = MCState Shore Boat Shore deriving (Eq, Show, Ord)

isGoal :: MCState -> Bool
isGoal (MCState _ _ (Shore (3, 3))) = True
isGoal _ = False

gen :: MCState -> [MCState]
gen (MCState (Shore (lm, lc)) (Boat (b, bm, bc)) (Shore (rm, rc))) =
  let rowBoat = if (bm + bc) == 0 then [] else
                  [MCState (Shore (lm, lc)) (Boat (not b, bm, bc)) (Shore (rm, rc))]

      putBoatA = if b && bm == 0 && rm /= 0 then
                   [MCState (Shore (lm, lc)) (Boat (b, bm + 1, bc)) (Shore (rm - 1, rc))]
                 else []
      putBoatB = if b && bc == 0 && rc /= 0 then
                   [MCState (Shore (lm, lc)) (Boat (b, bm, bc + 1)) (Shore (rm, rc - 1))]
                 else []
      getBoatA = if b && bm /= 0 then
                   [MCState (Shore (lm, lc)) (Boat (b, bm - 1, bc)) (Shore (rm + 1, rc))]
                 else []
      getBoatB = if b && bc /= 0 then
                   [MCState (Shore (lm, lc)) (Boat (b, bm, bc - 1)) (Shore (rm, rc + 1))]
                 else []

      putBoatC = if not b && bm == 0 && lm /= 0 then
                   [MCState (Shore (lm - 1, lc)) (Boat (b, bm + 1, bc)) (Shore (rm, rc))]
                 else []
      putBoatD = if not b && bc == 0 && lc /= 0 then
                   [MCState (Shore (lm, lc - 1)) (Boat (b, bm, bc + 1)) (Shore (rm, rc))]
                 else []
      getBoatC = if not b && bm /= 0 then
                   [MCState (Shore (lm + 1, lc)) (Boat (b, bm - 1, bc)) (Shore (rm, rc))]
                 else []
      getBoatD = if not b && bc /= 0 then
                   [MCState (Shore (lm, lc + 1)) (Boat (b, bm, bc - 1)) (Shore (rm, rc))]
                 else []

      total = rowBoat ++ putBoatA ++ putBoatB ++ putBoatC ++ putBoatD ++
              getBoatA ++ getBoatB ++ getBoatC ++ getBoatD

      isValidState = (lm == 0 || lm >= lc) && (rm == 0 || rm >= rc)
  in if isValidState then total else []

mNc :: SearchProblem MCState
mNc = SearchProblem [MCState (Shore (3, 3)) (Boat (False, 0, 0)) (Shore (0, 0))] gen isGoal
