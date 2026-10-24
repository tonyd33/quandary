module Quandary.Core.Solver
  ( Solver (..)
  , Query (..)
  , Sat (..)
  , Solve
  , fromSat
  , Consequences (..)
  , consequences
  , backbone
  , core
  , reaches
  )
where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Types

data Consequences
  = -- | No design holds the literals.
    Stale
  | Reaches (Map OptionId Verdict)
  deriving (Eq, Show)

data Query v m = Query
  { variables :: Set v
  , design :: Set (Lit v) -> m (Maybe (Set v))
  -- ^ Returns the variables a design holding the literals takes, or Nothing
  -- when none does.
  }

newtype Solver m = Solver
  { solving :: forall v a. Ord v => Formula v -> (Query v m -> m a) -> m a
  -- ^ Run queries on one solver holding the formula.
  }

-- | Returns the variables true in a model of the clauses under the
-- assumptions, or Nothing when there is none.
type Solve m = [Int] -> m (Maybe (Int -> Bool))

newtype Sat m = Sat
  { withClauses :: forall a. Int -> [[Int]] -> (Solve m -> m a) -> m a
  -- ^ Run solves on one solver holding the clauses over variables 1 to n.
  }

-- | Answer queries through a SAT backend.
fromSat :: Monad m => Sat m -> Solver m
fromSat backend = Solver {solving = satSession backend}

satSession :: (Ord v, Monad m) => Sat m -> Formula v -> (Query v m -> m a) -> m a
satSession (Sat withClauses) f k = withClauses (Map.size n.numbers) n.clauses (k . satQuery f n)
  where
    n = numbered f

satQuery :: (Ord v, Applicative m) => Formula v -> Numbered v -> Solve m -> Query v m
satQuery f n solve = Query {variables = f.variables, design}
  where
    design s = case signed n.numbers s of
      Nothing -> pure Nothing
      Just ls -> fmap (decoded n) <$> solve ls

-- | Holds when a design holds the literals.
reaches :: Functor m => Query v m -> Set (Lit v) -> m Bool
reaches query = fmap isJust . query.design

-- | Returns the verdict on each variable over the designs holding the
-- literals, or Stale when none does.
consequences :: Monad m => Query OptionId m -> Set Literal -> m Consequences
consequences query held = maybe Stale (Reaches . verdicts) <$> backbone query held (literalsOver query.variables)
  where
    verdicts forced = Map.fromSet (verdict forced) query.variables
    verdict forced o
      | Set.member (Take o) forced = Forced
      | Set.member (Refuse o) forced = Impossible
      | otherwise = Possible

literalsOver :: Ord v => Set v -> Set (Lit v)
literalsOver vs = Set.map Take vs <> Set.map Refuse vs

-- | Returns the candidates every design holding the literals holds, or
-- Nothing when none does, querying only a candidate every design found so
-- far holds.
backbone :: (Ord v, Monad m) => Query v m -> Set (Lit v) -> Set (Lit v) -> m (Maybe (Set (Lit v)))
backbone query held candidates = query.design held >>= traverse (\d -> go Set.empty (Set.filter (holdsIn d) candidates))
  where
    go found open = case Set.minView open of
      Nothing -> pure found
      Just (l, rest) ->
        query.design (Set.insert (opposite l) held) >>= \case
          Nothing -> go (Set.insert l found) rest
          Just d -> go found (Set.filter (holdsIn d) rest)

holdsIn :: Ord v => Set v -> Lit v -> Bool
holdsIn d (Take v) = Set.member v d
holdsIn d (Refuse v) = Set.notMember v d

-- | Returns a minimal subset of the candidates that no design holds
-- alongside the fixed literals, or Nothing when one does.
core :: (Ord v, Monad m) => Query v m -> Set (Lit v) -> Set (Lit v) -> m (Maybe (Set (Lit v)))
core query candidates fixed = do
  reached <- reaches query (Set.union fixed candidates)
  if reached then pure Nothing else Just <$> minimise blocked candidates
  where
    blocked = fmap not . reaches query . Set.union fixed

-- | Drop each element in order whose removal keeps the predicate true.
--
-- Preconditions:
--
-- * the predicate holds of the given set;
-- * it holds of every superset of a set it holds of.
--
-- Postconditions: it holds of the result and of no set one element smaller.
minimise :: (Ord a, Monad m) => (Set a -> m Bool) -> Set a -> m (Set a)
minimise holds s = foldM step s (Set.toList s)
  where
    step kept x = do
      let without = Set.delete x kept
      stillHolds <- holds without
      pure (if stillHolds then without else kept)

-- | The formula over its variables numbered from 1 in order, as
-- clauses of signed variable numbers.
data Numbered v = Numbered
  { numbers :: Map v Int
  , clauses :: [[Int]]
  }

numbered :: Ord v => Formula v -> Numbered v
numbered f = Numbered {numbers, clauses = map clause (Set.toList f.forbidden)}
  where
    numbers = Map.fromList (zip (Set.toList f.variables) [1 ..])
    clause forbid = [negate (number numbers l) | l <- Set.toList forbid]

-- | Returns the literals as signed variable numbers, or Nothing when one
-- names a variable the formula lacks.
signed :: Ord v => Map v Int -> Set (Lit v) -> Maybe [Int]
signed numbers = traverse known . Set.toList
  where
    known l
      | Map.member (optionOf l) numbers = Just (number numbers l)
      | otherwise = Nothing

number :: Ord v => Map v Int -> Lit v -> Int
number numbers (Take v) = numbers Map.! v
number numbers (Refuse v) = negate (numbers Map.! v)

decoded :: Numbered v -> (Int -> Bool) -> Set v
decoded n model = Map.keysSet (Map.filter model n.numbers)
