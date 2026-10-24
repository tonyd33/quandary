module Gen
  ( genSpace
  , genGraded
  , genPoint
  , generalize
  , specialize
  , designs
  , directlyValid
  , designsReaching
  )
where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Fixtures (listed, option, other, question, spaceOf)
import Quandary.Core.Compile (compile)
import Quandary.Core.Types
import Test.QuickCheck

-- | Generate up to three questions of up to three options, raised only by
-- options of earlier questions.
genSpace :: Gen Space
genSpace = do
  shape <- listOf1 (chooseInt (0, 3)) `suchThat` ((<= 3) . length)
  let ids = [(qid i, [oid i j | j <- [0 .. n - 1]]) | (i, n) <- zip [0 :: Int ..] shape]
      pool = concat [other q : map listed os | (q, os) <- ids]
      earlier i = [listed o | (_, os) <- take i ids, o <- os]
  spaceOf <$> sequence [genQuestion pool (earlier i) q os | (i, (q, os)) <- zip [0 ..] ids]
  where
    qid i = Text.pack ("q" <> show i)
    oid i j = Text.pack ("o" <> show i <> show j)

-- | Declare three grades and assess some options, a few unverified.
genGraded :: Space -> Gen Space
genGraded space = do
  qs <- mapM (\q -> (\os -> q {options = os}) <$> mapM assess q.options) space.questions
  pure space {grades = grades, questions = qs}
  where
    grades = ["low", "mid", "high"]
    assess o = do
      a <- frequency [(1, pure Nothing), (2, Just <$> (Assessment <$> elements grades <*> frequency [(3, pure False), (1, pure True)] <*> pure Nothing))]
      pure o {assessment = a}

genQuestion :: [OptionId] -> [OptionId] -> Text -> [Text] -> Gen Question
genQuestion pool raisers q os = do
  pick <- elements [One, Some, Any]
  raisedBy <- frequency [(1, pure []), (1, sublistOf' 2 raisers >>= mapM (\r -> (r :|) <$> frequency [(2, pure []), (1, sublistOf' 1 raisers)]))]
  options <- mapM (genOption pool) os
  forbid <- frequency [(3, pure []), (1, (: []) <$> genForbid pool)]
  pure (question q pick raisedBy options) {forbid}

genOption :: [OptionId] -> Text -> Gen Option
genOption pool o = do
  needs <- frequency [(3, pure []), (1, sublistOf' 1 pool)]
  excludes <- frequency [(3, pure []), (1, sublistOf' 1 pool)]
  needsOneOf <- frequency [(4, pure Nothing), (1, nonEmpty <$> sublistOf' 2 pool)]
  pure (option o) {needs, needsOneOf, excludes}

genForbid :: [OptionId] -> Gen Forbid
genForbid pool = do
  n <- chooseInt (1, 3)
  ls <- replicateM n (genLiteral pool)
  pure Forbid {literals = Set.fromList ls, why = Nothing}

genLiteral :: [OptionId] -> Gen Literal
genLiteral pool = elements [Take, Refuse] <*> elements pool

-- | Generate up to three rulings over the space's options.
genPoint :: Space -> Gen (Set Literal)
genPoint space = do
  n <- chooseInt (0, 3)
  Set.fromList <$> replicateM n (genLiteral (Set.toList (compile space).variables))

sublistOf' :: Int -> [a] -> Gen [a]
sublistOf' _ [] = pure []
sublistOf' n xs = do
  k <- chooseInt (1, n)
  replicateM k (elements xs)

-- | Apply an edit that only adds designs.
generalize :: Space -> Gen Space
generalize space = do
  q <- elements (Map.elems space.questions)
  let pool = Set.toList (compile space).variables
  q' <-
    oneof $
      [ do
          o <- genOption pool "new"
          pure q {options = q.options ++ [o]}
      , pure q {pick = Any}
      , pure q {forbid = drop 1 q.forbid}
      ]
        ++ [ do
               o <- elements q.options
               o' <- elements [o {needs = []}, o {excludes = []}, o {needsOneOf = Nothing}]
               pure q {options = map (\p -> if p.id == o.id then o' else p) q.options}
           | not (null q.options)
           ]
  pure space {questions = Map.insert q.id q' space.questions}

-- | Apply an edit that only removes designs.
specialize :: Space -> Gen Space
specialize space = do
  q <- elements (Map.elems space.questions)
  let pool = Set.toList (compile space).variables
  q' <-
    oneof $
      [ do
          f <- genForbid pool
          pure q {forbid = f : q.forbid}
      , pure q {pick = One}
      ]
        ++ [ do
               o <- elements q.options
               x <- elements pool
               let o' = o {needs = x : o.needs}
               pure q {options = map (\p -> if p.id == o.id then o' else p) q.options}
           | not (null q.options)
           ]
  pure space {questions = Map.insert q.id q' space.questions}

-- | Returns every assignment of the space's options.
designs :: Space -> [Set Literal]
designs space = foldr each [Set.empty] (Set.toList (compile space).variables)
  where
    each o ds = [Set.insert l d | d <- ds, l <- [Take o, Refuse o]]

-- | Decide validity from the semantics' prose, independently of 'compile'.
directlyValid :: Space -> Set Literal -> Bool
directlyValid space d = all holds (Map.elems space.questions)
  where
    taken o = Set.member (Take o) d
    holds q =
      answers q
        && all respected q.options
        && not (any (all (`Set.member` d) . (.literals)) q.forbid)
    answers q
      | not (live q) = not (any taken (choices q))
      | otherwise = case q.pick of
          One -> length (filter taken (choices q)) == 1
          Some -> any taken (choices q)
          Any -> True
    live q = null q.raisedBy || any (all taken) q.raisedBy
    respected o =
      not (taken (Listed o.id))
        || ( all taken o.needs
               && not (any taken o.excludes)
               && maybe True (any taken) o.needsOneOf
           )

-- | Returns every valid design holding the rulings.
designsReaching :: Space -> Set Literal -> [Set Literal]
designsReaching space p = [d | d <- designs space, directlyValid space d, p `Set.isSubsetOf` d]
