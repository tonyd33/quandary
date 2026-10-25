module Quandary.Core.Solver
  ( Solver (..)
  , Query (..)
  , Sat (..)
  , Solve
  , fromSat
  , Consequences (..)
  , design
  , encoded
  , reachesSigned
  , consequences
  , backbone
  , core
  , reaches
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Types

data Consequences
  = -- | No design holds the literals.
    Stale
  | Reaches (Map OptionId Verdict)
  deriving (Eq, Show)

-- | A solver session over a formula's variables.
data Query v m = Query
  { variables :: Set v
  , number :: v -> Maybe Int
  -- ^ Nothing for a variable the formula lacks.
  , variable :: Int -> Maybe v
  -- ^ Nothing for a number no variable of the formula has.
  , solve :: Solve m
  }

data Solver m = Solver
  { solving :: forall v a. Ord v => Formula v -> (Query v m -> m a) -> m a
  -- ^ Run queries on one solver holding the formula.
  , width :: Int
  -- ^ How many sessions `parallel` runs at once.
  , parallel :: forall a. [m a] -> m [a]
  -- ^ Run the actions, at once where the solver can.
  }

-- | Returns the given variables true in a model of the clauses under the
-- assumptions, or Nothing when there is none.
type Solve m = [Int] -> [Int] -> m (Maybe IntSet)

data Sat m = Sat
  { withClauses :: forall a. Int -> [[Int]] -> (Solve m -> m a) -> m a
  -- ^ Run solves on one solver holding the clauses over variables 1 to n.
  , width :: Int
  , parallel :: forall a. [m a] -> m [a]
  }

-- | Answer queries through a SAT backend.
fromSat :: Monad m => Sat m -> Solver m
fromSat backend@Sat {width, parallel} = Solver {solving = satSession backend, width, parallel}

satSession :: (Ord v, Monad m) => Sat m -> Formula v -> (Query v m -> m a) -> m a
satSession Sat {withClauses} f k = withClauses (Map.size n.numbers) n.clauses (k . satQuery f n)
  where
    n = numbered f

satQuery :: Ord v => Formula v -> Numbered v -> Solve m -> Query v m
satQuery f n solve =
  Query
    { variables = f.variables
    , number = (`Map.lookup` n.numbers)
    , variable = (`IntMap.lookup` n.names)
    , solve
    }

-- | Returns the given variables a design holding both sets of literals
-- takes, or Nothing when none does. Bind `design query held` once for the
-- literals every call holds; each application encodes them.
design :: (Ord v, Applicative m) => Query v m -> Set (Lit v) -> Set (Lit v) -> Set v -> m (Maybe (Set v))
design query held = withHeld (encoded query held)
  where
    withHeld Nothing _ _ = pure Nothing
    withHeld (Just hs) extra vs = case encoded query extra of
      Nothing -> pure Nothing
      Just ls -> fmap (decoded query) <$> query.solve (ls ++ hs) (mapMaybe query.number (Set.toList vs))

-- | Returns the literals as signed variable numbers, or Nothing when one
-- names a variable the query lacks.
encoded :: Query v m -> Set (Lit v) -> Maybe [Int]
encoded query = traverse (literalNumber query) . Set.toList

literalNumber :: Query v m -> Lit v -> Maybe Int
literalNumber query (Take v) = query.number v
literalNumber query (Refuse v) = negate <$> query.number v

literalOf :: Query v m -> Int -> Maybe (Lit v)
literalOf query n
  | n > 0 = Take <$> query.variable n
  | otherwise = Refuse <$> query.variable (negate n)

decoded :: Ord v => Query v m -> IntSet -> Set v
decoded query = Set.fromList . mapMaybe query.variable . IntSet.toList

-- | Holds when a design holds the signed literals.
reachesSigned :: Functor m => Query v m -> [Int] -> m Bool
reachesSigned query ls = isJust <$> query.solve ls []

-- | Holds when a design holds the literals.
reaches :: Applicative m => Query v m -> Set (Lit v) -> m Bool
reaches query held = maybe (pure False) (reachesSigned query) (encoded query held)

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
--
-- Precondition: every candidate names a variable of the query.
backbone :: (Ord v, Monad m) => Query v m -> Set (Lit v) -> Set (Lit v) -> m (Maybe (Set (Lit v)))
backbone query held candidates = case encoded query held of
  Nothing -> pure Nothing
  Just hs -> fmap decode <$> signedBackbone query.solve hs known
  where
    known = IntSet.fromList (mapMaybe (literalNumber query) (Set.toList candidates))
    decode found = Set.fromList (mapMaybe (literalOf query) (IntSet.toList found))

-- | Returns the signed candidates every design holding the signed literals
-- holds, or Nothing when none does.
signedBackbone :: Monad m => Solve m -> [Int] -> IntSet -> m (Maybe IntSet)
signedBackbone solve hs candidates = solve hs (variablesOf candidates) >>= traverse (go IntSet.empty . (`IntSet.filter` candidates) . holdsIn)
  where
    go found open = case IntSet.minView open of
      Nothing -> pure found
      Just (l, rest) ->
        solve (negate l : hs) (variablesOf rest) >>= \case
          Nothing -> go (IntSet.insert l found) rest
          Just d -> go found (IntSet.filter (holdsIn d) rest)

variablesOf :: IntSet -> [Int]
variablesOf = IntSet.toList . IntSet.map abs

-- | Holds when the design, given as the variables it takes, holds the
-- signed literal.
holdsIn :: IntSet -> Int -> Bool
holdsIn d l
  | l > 0 = IntSet.member l d
  | otherwise = IntSet.notMember (negate l) d

-- | Returns a minimal subset of the candidates that no design holds
-- alongside the fixed literals, or Nothing when one does.
core :: (Ord v, Monad m) => Query v m -> Set (Lit v) -> Set (Lit v) -> m (Maybe (Set (Lit v)))
core query candidates fixed = do
  reached <- reachesWith candidates
  if reached then pure Nothing else Just <$> minimise (fmap not . reachesWith) candidates
  where
    within = design query fixed
    reachesWith s = isJust <$> within s Set.empty

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
  , names :: IntMap v
  , clauses :: [[Int]]
  }

numbered :: Ord v => Formula v -> Numbered v
numbered f = Numbered {numbers, names, clauses = map clause (Set.toList f.forbidden)}
  where
    numbers = Map.fromList (zip (Set.toList f.variables) [1 ..])
    names = IntMap.fromList (zip [1 ..] (Set.toList f.variables))
    clause forbid = [negate (signedIn numbers l) | l <- Set.toList forbid]

-- | Returns the literal's signed number. Every forbidden set uses only the
-- formula's variables.
signedIn :: Ord v => Map v Int -> Lit v -> Int
signedIn numbers (Take v) = numbers Map.! v
signedIn numbers (Refuse v) = negate (numbers Map.! v)
