module Quandary.Core.Compile
  ( Constraint (..)
  , compile
  , constraints
  , options
  )
where

import Data.Foldable (toList)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Types

-- | One rule of a space that a valid design keeps.
data Constraint
  = -- | Taking the first option takes the second.
    Needs OptionId OptionId
  | -- | No design takes both options.
    Excludes OptionId OptionId
  | -- | Taking the option takes one of the others.
    NeedsOneOf OptionId (NonEmpty OptionId)
  | -- | The question's options are taken only while one of its raiser
    -- sets is.
    RaisedFirst Id [OptionId] [NonEmpty OptionId]
  | -- | The question takes at most one of its options.
    AtMostOne Id [OptionId]
  | -- | The question takes one of its options while one of its raiser sets
    -- is taken, always when it has none.
    AtLeastOne Id [OptionId] [NonEmpty OptionId]
  | -- | The question's `forbid` entry holds in no design.
    Forbidden Id Forbid
  deriving (Eq, Ord, Show)

compile :: Space -> Formula OptionId
compile space =
  Formula
    { variables = options space
    , forbidden = Set.fromList (concatMap snd (constraints space))
    }

options :: Space -> Set OptionId
options space = Set.fromList (concatMap choices (Map.elems space.questions))

-- | Returns each constraint of the space with the sets of literals it
-- forbids.
constraints :: Space -> [(Constraint, [Set Literal])]
constraints space = concatMap questionConstraints (Map.elems space.questions)

questionConstraints :: Question -> [(Constraint, [Set Literal])]
questionConstraints q =
  [(Forbidden q.id f, [f.literals]) | f <- q.forbid]
    ++ concatMap optionConstraints q.options
    ++ [(RaisedFirst q.id (listedThenOther q) q.raisedBy, liveness) | not (null q.raisedBy)]
    ++ picking
  where
    liveness = [Set.fromList (Take o : map Refuse unraised) | o <- choices q, unraised <- traverse toList q.raisedBy]
    picking = case q.pick of
      Any -> []
      Some -> [(AtLeastOne q.id (listedThenOther q) q.raisedBy, atLeastOneWhileLive)]
      One ->
        [(AtMostOne q.id (listedThenOther q), atMostOne), (AtLeastOne q.id (listedThenOther q) q.raisedBy, atLeastOneWhileLive)]
    atMostOne = [Set.fromList [Take o, Take p] | o : ps <- tails (choices q), p <- ps]
    atLeastOneWhileLive
      | null q.raisedBy = [refuseAll]
      | otherwise = [Set.fromList (map Take (toList r)) <> refuseAll | r <- q.raisedBy]
    refuseAll = Set.fromList (map Refuse (choices q))

optionConstraints :: Option -> [(Constraint, [Set Literal])]
optionConstraints o =
  [(Excludes self x, [with [Take x]]) | x <- o.excludes]
    ++ [(Needs self x, [with [Refuse x]]) | x <- o.needs]
    ++ [(NeedsOneOf self xs, [with (map Refuse (toList xs))]) | Just xs <- [o.needsOneOf]]
  where
    self = Listed o.id
    with ls = Set.fromList (Take self : ls)
