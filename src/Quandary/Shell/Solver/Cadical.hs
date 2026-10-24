{-# LANGUAGE CApiFFI #-}

module Quandary.Shell.Solver.Cadical (cadical) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Quandary.Core.Solver (Sat (..), Solve, Solver, fromSat)

data CCaDiCaL

foreign import capi unsafe "ccadical.h ccadical_init" initSolver :: IO (Ptr CCaDiCaL)

foreign import capi unsafe "ccadical.h ccadical_release" release :: Ptr CCaDiCaL -> IO ()

foreign import capi unsafe "ccadical.h ccadical_add" add :: Ptr CCaDiCaL -> CInt -> IO ()

foreign import capi unsafe "ccadical.h ccadical_assume" assume :: Ptr CCaDiCaL -> CInt -> IO ()

foreign import capi unsafe "ccadical.h ccadical_solve" solve :: Ptr CCaDiCaL -> IO CInt

foreign import capi unsafe "ccadical.h ccadical_val" val :: Ptr CCaDiCaL -> CInt -> IO CInt

-- | Answers from CaDiCaL, one solver per session.
cadical :: Solver IO
cadical = fromSat Sat {withClauses = session}

-- | Run solves on a fresh solver holding the clauses.
session :: Int -> [[Int]] -> (Solve IO -> IO a) -> IO a
session n clauses k = bracket initSolver release $ \solver -> do
  forM_ clauses $ \c -> do
    forM_ c (add solver . fromIntegral)
    add solver 0
  k $ \ls -> do
    forM_ ls (assume solver . fromIntegral)
    result <- solve solver
    if result /= 10
      then pure Nothing
      else Just . flip IntSet.member <$> trueVariables solver n

-- | Returns the variables 1 to n true in the solver's current model.
trueVariables :: Ptr CCaDiCaL -> Int -> IO IntSet
trueVariables solver n = IntSet.fromList <$> filterM (\v -> (> 0) <$> val solver (fromIntegral v)) [1 .. n]
