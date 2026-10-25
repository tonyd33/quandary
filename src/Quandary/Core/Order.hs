module Quandary.Core.Order (decisionOrder) where

import Data.Foldable (toList)
import Data.Graph qualified as Graph
import Data.List (groupBy, minimumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Quandary.Core.Types

-- | Returns the given questions in the order to decide them, in runs of
-- one block.
--
-- A question goes once every question of its raisers has gone. Of the
-- questions free to go, the next is the one touched by the most
-- constraints and raisings, then one of the block just left, then the one
-- with the lower id. Questions joined by a constraint form a block. When
-- a cycle of raising leaves no question free, any may go.
decisionOrder :: Space -> Set Id -> [[Id]]
decisionOrder space open = map (map snd) (groupBy (\a b -> fst a == fst b) (go Nothing open))
  where
    questionAt = questionOf space
    constraints =
      [ Set.fromList (mapMaybe questionAt (Listed o.id : targets))
      | q <- Map.elems space.questions
      , o <- q.options
      , let targets = o.needs ++ maybe [] toList o.needsOneOf ++ o.excludes
      , not (null targets)
      ]
        ++ [ Set.fromList (mapMaybe (questionAt . optionOf) (Set.toList f.literals))
           | q <- Map.elems space.questions
           , f <- q.forbid
           ]
    raisings = [Set.fromList (q.id : mapMaybe questionAt (toList r)) | q <- Map.elems space.questions, r <- q.raisedBy]
    blockOf = components open constraints
    weight = Map.fromListWith (+) [(q, 1 :: Int) | c <- constraints ++ raisings, q <- Set.toList c]
    raisers q = maybe [] (mapMaybe questionAt . concatMap toList . (.raisedBy)) (Map.lookup q space.questions)
    go :: Maybe Id -> Set Id -> [(Id, Id)]
    go current remaining
      | Set.null remaining = []
      | otherwise = (b, q) : go (Just b) (Set.delete q remaining)
      where
        ready = [r | r <- Set.toList remaining, all (`Set.notMember` remaining) (filter (/= r) (raisers r))]
        candidates = if null ready then Set.toList remaining else ready
        q =
          minimumBy (comparing (\r -> (Down (Map.findWithDefault 0 r weight), current /= Just (blockOf Map.! r), r))) candidates
        b = blockOf Map.! q

-- | Returns each question's block: the least question it is joined to
-- through groups' members among the questions.
components :: Set Id -> [Set Id] -> Map Id Id
components qs groups = Map.fromList [(q, Set.findMin block) | block <- blocks, q <- Set.toList block]
  where
    blocks = [Set.fromList (map (`Set.elemAt` qs) (toList t)) | t <- Graph.components graph]
    graph =
      Graph.buildG
        (0, Set.size qs - 1)
        [ (a, b)
        | g <- groups
        , let members = [Set.findIndex q qs | q <- Set.toList (Set.intersection g qs)]
        , a <- take 1 members
        , b <- members
        ]
