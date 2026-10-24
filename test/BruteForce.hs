module BruteForce (bruteForce, bruteForceOn) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Solver (Query (..), Solver (..))
import Quandary.Core.Types

-- | Search the designs by backtracking. Exponential in the variables.
bruteForce :: Applicative m => Solver m
bruteForce = Solver {solving = search}

bruteForceOn :: Ord v => Formula v -> (Query v Identity -> Identity a) -> a
bruteForceOn f k = runIdentity (search f k)

search :: (Ord v, Applicative m) => Formula v -> (Query v m -> m a) -> m a
search f k = k Query {variables = f.variables, design}
  where
    design s = pure (taken <$> firstDesign f s)

-- | Returns the first design holding the literals, deciding the variables
-- in order, taken first.
firstDesign :: Ord v => Formula v -> Set (Lit v) -> Maybe (Set (Lit v))
firstDesign f held
  | any unknown held = Nothing
  | violates f held = Nothing
  | otherwise = go (filter (not . decides held) (Set.toList f.variables)) held
  where
    unknown l = Set.notMember (optionOf l) f.variables
    go [] d = Just d
    go (v : vs) d = extend (Take v) <|> extend (Refuse v)
      where
        extend l
          | violates f d' = Nothing
          | otherwise = go vs d'
          where
            d' = Set.insert l d

decides :: Ord v => Set (Lit v) -> v -> Bool
decides d v = Set.member (Take v) d || Set.member (Refuse v) d

violates :: Ord v => Formula v -> Set (Lit v) -> Bool
violates f d = any (`Set.isSubsetOf` d) f.forbidden || contradicts d

contradicts :: Ord v => Set (Lit v) -> Bool
contradicts d = any (\l -> Set.member (opposite l) d) d

taken :: Ord v => Set (Lit v) -> Set v
taken d = Set.fromList [v | Take v <- Set.toList d]
