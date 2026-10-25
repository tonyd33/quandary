module Toysolver (toysolver) where

import Data.IntSet qualified as IntSet
import Quandary.Core.Solver (Sat (..), Solve, Solver, fromSat)
import ToySolver.SAT qualified as SAT

-- | Answers from a CDCL solver, one per session.
toysolver :: Solver IO
toysolver = fromSat Sat {withClauses = session, width = 1, parallel = sequence}

-- | Run solves on a fresh solver holding the clauses.
session :: Int -> [[Int]] -> (Solve IO -> IO a) -> IO a
session n clauses k = do
  solver <- SAT.newSolver
  SAT.newVars_ solver n
  mapM_ (SAT.addClause solver) clauses
  k $ \ls vs -> do
    reached <- SAT.solveWith solver ls
    if not reached
      then pure Nothing
      else (\m -> Just (IntSet.fromList (filter (SAT.evalVar m) vs))) <$> SAT.getModel solver
