module Properties (properties) where

import BruteForce (bruteForceOn)
import Control.Monad (forM)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Fixtures (modelAt)
import Gen
import Quandary.Core.Analyse (Analysis (..), Bounds (..), Implication (..), Moved (..), QuestionState (..), Standing (..), Supposition (..), analyse, bounds, dominance, implications, whatif)
import Quandary.Core.Compile (compile)
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Shell.Solver.Cadical (cadical)
import Test.Tasty
import Test.Tasty.QuickCheck
import Toysolver (toysolver)

properties :: TestTree
properties =
  testGroup
    "semantics"
    [ testProperty "forbidden sets agree with the prose" $
        forAll genSpace $ \space ->
          let f = compile space
              valid d = not (any (`Set.isSubsetOf` d) f.forbidden)
           in conjoin [counterexample (show d) (valid d === directlyValid space d) | d <- designs space]
    , testProperty "a generalization only weakens verdicts" $
        forAll genSpace $ \space ->
          forAll (generalize space) $ \space' ->
            forAll (genPoint space) $ \p ->
              weakens (consequencesOf space p) (consequencesOf space' p)
    , testProperty "a specialization only strengthens verdicts" $
        forAll genSpace $ \space ->
          forAll (specialize space) $ \space' ->
            forAll (genPoint space) $ \p ->
              weakens (consequencesOf space' p) (consequencesOf space p)
    , testProperty "a fresh point forces its rulings" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p -> case consequencesOf space p of
            Stale -> discard
            Reaches vs -> conjoin [holds (Map.lookup (optionOf l) vs) l | l <- Set.toList p]
    , testProperty "why is a minimal subset" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            forAll (elements (Set.toList (compile space).variables)) $ \o ->
              let f = compile space
                  reaching s = bruteForceOn f (`reaches` s)
               in case bruteForceOn f (\q -> core q p (Set.singleton (Take o))) of
                    Nothing -> property (reaching (Set.insert (Take o) p))
                    Just e ->
                      conjoin
                        [ property (e `Set.isSubsetOf` p)
                        , property (not (reaching (Set.insert (Take o) e)))
                        , conjoin [property (reaching (Set.insert (Take o) (Set.delete l e))) | l <- Set.toList e]
                        ]
    , testProperty "bounds are the best design taking each option" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            consistent p ==>
              ioProperty . fmap conjoin . forM [toysolver, cadical] $ \solver ->
                analyse solver (modelAt space p) "current" >>= \case
                  StaleAt _ -> pure (bestDesigns space p === Map.empty)
                  Fresh s -> (=== bestDesigns space p) <$> bounds solver (modelAt space p) "current" s (Set.toList (compile space).variables)
    , testProperty "an option is dominated when its upper bound is below a sibling's lower bound" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            consistent p ==>
              ioProperty $
                analyse cadical (modelAt space p) "current" >>= \case
                  StaleAt _ -> pure discard
                  Fresh s -> do
                    bs <- bounds cadical (modelAt space p) "current" s (Set.toList (compile space).variables)
                    let dominated = dominance (modelAt space p) "current" bs
                        best = bestDesigns space p
                        sibling o o' = o /= o' && questionOf space o == questionOf space o'
                        below o o' = (best Map.! o).hi < (best Map.! o').lo
                    pure $
                      Map.keysSet dominated === Set.fromList [o | o <- Map.keys best, any (\o' -> sibling o o' && below o o') (Map.keys best)]
                        .&&. conjoin [counterexample (show (o, o')) (sibling o o' && below o o') | (o, o') <- Map.toList dominated]
    , testProperty "whatif accept bounds each question the option raises at the point it reaches" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            forAll (elements (Set.toList (compile space).variables)) $ \o ->
              let p' = Set.insert (Take o) (Set.delete (Refuse o) p)
               in consistent p' ==>
                    ioProperty . fmap conjoin . forM [toysolver, cadical] $ \solver ->
                      whatif solver (modelAt space p) "current" [Suppose (Take o)] >>= \case
                        Left e -> pure (counterexample (show e) False)
                        Right (_, Moved {after = StaleAt _}) -> pure (property True)
                        Right (m', Moved {after = Fresh s}) -> do
                          found <- implications solver m' "current" s (Set.singleton o)
                          pure $
                            map (\i -> i.raising.question.id) found === [q.id | q <- Map.elems space.questions, any (elem o) q.raisedBy]
                              .&&. conjoin [i.values === raisedBounds space p' s i.raising.question | i <- found]
    ]
  where
    consistent p = not (any (\o -> Set.member (Refuse o) p) [o | Take o <- Set.toList p])
    raisedBounds space p s q
      | Map.lookup q.id s.states == Just Dormant = Nothing
      | otherwise = Just (Map.restrictKeys (bestDesigns space p) (Set.fromList [c | c <- choices q, Map.lookup c s.verdicts == Just Possible]))
    consequencesOf space p = let f = compile space in bruteForceOn f (\q -> consequences q p)
    holds v (Take _) = v === Just Forced
    holds v (Refuse _) = v === Just Impossible

-- | Holds when every verdict of the second is no stronger than the first's.
weakens :: Consequences -> Consequences -> Property
weakens Stale _ = property True
weakens (Reaches _) Stale = counterexample "became stale" False
weakens (Reaches old) (Reaches new) =
  conjoin
    [ counterexample (show o <> ": " <> show b <> " became " <> show a) (a == Possible || a == b)
    | (o, a) <- Map.toList new
    , Just b <- [Map.lookup o old]
    ]

-- | Decide the bounds from the semantics' prose: over every valid design
-- holding the rulings and taking the option, the best lowest grade among
-- the option's commitments, ungraded options counted lowest, then highest.
bestDesigns :: Space -> Set Literal -> Map.Map OptionId Bounds
bestDesigns space p =
  Map.fromList
    [ (o, Bounds (best low) (best high))
    | o <- variables
    , let ds = [d | d <- reaching, Set.member (Take o) d]
    , not (null ds)
    , let seeds = Set.insert o (Set.difference (forcedIn ds) (forcedIn reaching))
          best g = maximum [minimum (map g (Set.toList (commits seeds d))) | d <- ds]
    ]
  where
    variables = Set.toList (compile space).variables
    reaching = designsReaching space p
    forcedIn ds = Set.fromList [v | v <- variables, all (Set.member (Take v)) ds]
    commits seeds d =
      let grow cs =
            Set.union cs $
              Set.fromList
                [ o
                | q <- Map.elems space.questions
                , any (\g -> all (\x -> Set.member (Take x) d) g && any (`Set.member` cs) g) q.raisedBy
                , o <- choices q
                , Set.member (Take o) d
                ]
          go cs = let cs' = grow cs in if cs' == cs then cs else go cs'
       in go seeds
    top = length space.grades - 1
    position g = length (takeWhile (/= g) space.grades)
    assessed = Map.fromList [(Listed o.id, a) | q <- Map.elems space.questions, o <- q.options, Just a <- [o.assessment]]
    low o = maybe 0 (\a -> if a.unverified then 0 else position a.grade) (Map.lookup o assessed)
    high o = maybe top (position . (.grade)) (Map.lookup o assessed)
