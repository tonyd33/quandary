module Solvers (solvers) where

import BruteForce (bruteForceOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Gen
import Quandary.Core.Compile (compile)
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Shell.Solver.Cadical (cadical)
import Test.Tasty
import Test.Tasty.QuickCheck
import Toysolver (toysolver)

solvers :: TestTree
solvers =
  testGroup
    "solvers"
    [ testProperty "brute force agrees with every design" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            let f = compile space in bruteForceOn f (\q -> consequences q p) === enumerated space p
    , agrees "toysolver" toysolver
    , agrees "cadical" cadical
    ]

agrees :: String -> Solver IO -> TestTree
agrees name solver =
  testGroup
    (name <> " agrees with brute force")
    [ testProperty "consequences" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            let f = compile space
             in ioProperty $ (=== bruteForceOn f (\q -> consequences q p)) <$> session solver f (\q -> consequences q p)
    , testProperty "core" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            let f = compile space
             in ioProperty . fmap conjoin . sequence $
                  [ counterexample (show l) . (=== bruteForceOn f (\q -> core q p l)) <$> session solver f (\q -> core q p l)
                  | l <- Set.empty : [Set.singleton d | o <- Set.toList f.variables, d <- [Take o, Refuse o]]
                  ]
    , testProperty "reaches" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            let f = compile space
             in ioProperty $ (=== bruteForceOn f (`reaches` p)) <$> session solver f (`reaches` p)
    ]

session :: Solver m -> Formula OptionId -> (Query OptionId m -> m a) -> m a
session Solver {solving} = solving

-- | Returns the verdicts by enumerating every design the prose admits.
enumerated :: Space -> Set.Set Literal -> Consequences
enumerated space p = case designsReaching space p of
  [] -> Stale
  ds -> Reaches (Map.fromSet (verdict ds) (compile space).variables)
  where
    verdict ds o
      | all (Set.member (Take o)) ds = Forced
      | any (Set.member (Take o)) ds = Possible
      | otherwise = Impossible
