module Quandary.Core.Compile (compile) where

import Data.Foldable (toList)
import Data.List (tails)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Types

compile :: Space -> Formula OptionId
compile space =
  Formula
    { variables = Set.fromList (concatMap choices qs)
    , forbidden = Set.fromList (concatMap questionSets qs)
    }
  where
    qs = Map.elems space.questions

questionSets :: Question -> [Set Literal]
questionSets q =
  map (.literals) q.forbid
    ++ concatMap optionSets q.options
    ++ liveness
    ++ picking
  where
    liveness
      | null q.raisedBy = []
      | otherwise = [Set.fromList (Take o : map Refuse unraised) | o <- choices q, unraised <- traverse toList q.raisedBy]
    picking = case q.pick of
      Any -> []
      Some -> atLeastOneWhileLive
      One -> atMostOne ++ atLeastOneWhileLive
    atMostOne = [Set.fromList [Take o, Take p] | o : ps <- tails (choices q), p <- ps]
    atLeastOneWhileLive
      | null q.raisedBy = [refuseAll]
      | otherwise = [Set.fromList (map Take (toList r)) <> refuseAll | r <- q.raisedBy]
    refuseAll = Set.fromList (map Refuse (choices q))

optionSets :: Option -> [Set Literal]
optionSets o =
  [with [Take x] | x <- o.excludes]
    ++ [with [Refuse x] | x <- o.needs]
    ++ [with (map Refuse (toList xs)) | Just xs <- [o.needsOneOf]]
  where
    with ls = Set.fromList (Take (Listed o.id) : ls)
