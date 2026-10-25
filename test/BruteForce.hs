module BruteForce (bruteForce, bruteForceOn) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity, runIdentity)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Quandary.Core.Solver (Query, Sat (..), Solve, Solver (..), fromSat)
import Quandary.Core.Types

-- | Search the designs by backtracking. Exponential in the variables.
bruteForce :: Monad m => Solver m
bruteForce = fromSat Sat {withClauses = search, width = 1, parallel = sequence}

bruteForceOn :: Ord v => Formula v -> (Query v Identity -> Identity a) -> a
bruteForceOn f k = case bruteForce of
  Solver {solving} -> runIdentity (solving f k)

search :: Applicative m => Int -> [[Int]] -> (Solve m -> m a) -> m a
search n clauses k = k (\assumed asked -> pure (IntSet.intersection (IntSet.fromList asked) <$> firstDesign n clauses assumed))

-- | Returns the variables the first design holding the literals takes,
-- deciding the variables in order, taken first.
firstDesign :: Int -> [[Int]] -> [Int] -> Maybe IntSet
firstDesign n clauses assumed
  | violates start = Nothing
  | otherwise = taken <$> go (filter (not . decides start) [1 .. n]) start
  where
    start = IntSet.fromList assumed
    go [] d = Just d
    go (v : vs) d = extend v <|> extend (negate v)
      where
        extend l
          | violates d' = Nothing
          | otherwise = go vs d'
          where
            d' = IntSet.insert l d
    violates d = any (all (\l -> IntSet.member (negate l) d)) clauses || contradicts d

-- | Holds when the signed literals hold a variable both ways.
contradicts :: IntSet -> Bool
contradicts d = any (\l -> IntSet.member (negate l) d) (IntSet.toList d)

decides :: IntSet -> Int -> Bool
decides d v = IntSet.member v d || IntSet.member (negate v) d

taken :: IntSet -> IntSet
taken = IntSet.filter (> 0)
