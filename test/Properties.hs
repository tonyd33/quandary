module Properties (properties) where

import BruteForce (bruteForce, bruteForceOn)
import Control.Monad (forM)
import Data.Functor.Identity (runIdentity)
import Data.List (sortOn, subsequences)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Fixtures (modelAt)
import Gen
import Quandary.Core.Analyse
  ( Analysis (..)
  , Bounds (..)
  , Cause (..)
  , Implication (..)
  , Moved (..)
  , QuestionState (..)
  , Standing (..)
  , Supposition (..)
  , analyse
  , bounds
  , causes
  , dominance
  , explain
  , implications
  , whatif
  )
import Quandary.Core.Compile (compile, constraints)
import Quandary.Core.Next (Lead (..), Move (..), leads)
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Core.View (OptionView (..), QuestionView (..), optionView, questionView, statusBounds)
import Quandary.Shell.Solver.Cadical (cadical)
import Test.Tasty
import Test.Tasty.QuickCheck
import Toysolver (toysolver)

-- | Returns the solver running three sessions at once.
threeWide :: Solver m -> Solver m
threeWide Solver {solving, parallel} = Solver {solving, width = 3, parallel}

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
    , testProperty "each constraint forbids what the prose says it does, and together they are validity" $
        forAll genSpace $ \space ->
          conjoin
            [ counterexample (show (c, d)) (not (any (`Set.isSubsetOf` d) lss) === directlyKeeps c d)
            | (c, lss) <- constraints space
            , d <- designs space
            ]
            .&&. conjoin
              [ counterexample (show d) (all (\(c, _) -> directlyKeeps c d) (constraints space) === directlyValid space d)
              | d <- designs space
              ]
    , testProperty "an explanation is a minimal set of constraints leaving no design" $
        forAll genSpace $ \space ->
          forAll (genPoint space) $ \p ->
            forAll (elements (Set.toList (compile space).variables)) $ \o ->
              let m = modelAt space p
                  noDesign cs ls = not (any (\d -> ls `Set.isSubsetOf` d && all (\c -> directlyKeeps c d) cs) (designs space))
                  opposing Forced = Refuse o
                  opposing _ = Take o
               in case bruteForceOn (compile space) (`consequences` p) of
                    Stale -> discard
                    Reaches vs -> case vs Map.! o of
                      Possible -> discard
                      v -> runIdentity $ do
                        rss <- causes bruteForce m "current" o v
                        found <- explain bruteForce space o v rss
                        pure $
                          conjoin
                            [ counterexample (show c) $
                                noDesign c.constraints (Set.insert (opposing v) c.rulings)
                                  .&&. conjoin
                                    [ property (not (noDesign (filter (/= k) c.constraints) (Set.insert (opposing v) c.rulings)))
                                    | k <- c.constraints
                                    ]
                            | c <- found
                            ]
    , testProperty "bounds are the best design taking each option" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            consistent p ==>
              ioProperty . fmap conjoin . forM [toysolver, cadical, threeWide cadical] $ \solver ->
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
    , testProperty "status bounds and question rows agree under brute force and CaDiCaL" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            let m = modelAt space p
                facts solver = do
                  a <- analyse solver m "current"
                  bs <- statusBounds solver m "current" a
                  vs <- mapM (questionView solver m "current" a) (Map.elems space.questions)
                  pure (a, bs, map (.rows) vs)
             in ioProperty ((===) <$> facts bruteForce <*> facts cadical)
    , testProperty "next names the fewest grades moving each root option's bounds" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            consistent p ==>
              ioProperty . fmap conjoin . forM [toysolver, cadical] $ \solver ->
                analyse solver (modelAt space p) "current" >>= \case
                  StaleAt _ -> pure discard
                  Fresh s -> do
                    found <- leads solver (modelAt space p) "current" s (length (rootsOf space s))
                    pure $
                      map (\l -> (l.option, l.move, Set.size l.grading)) found === fewestGrades space p (rootsOf space s)
                        .&&. conjoin (map (leadMoves space p) found)
    , testProperty "option views agree under brute force and CaDiCaL" $
        forAll (genSpace >>= genGraded) $ \space ->
          forAll (genPoint space) $ \p ->
            let m = modelAt space p
                facts solver = do
                  a <- analyse solver m "current"
                  vs <- sequence [optionView solver m "current" a q o | q <- Map.elems space.questions, o <- choices q]
                  pure [(v.row, v.causes) | v <- vs]
             in ioProperty ((===) <$> facts bruteForce <*> facts cadical)
    ]
  where
    consistent p = not (any (\o -> Set.member (Refuse o) p) [o | Take o <- Set.toList p])
    raisedBounds space p s q
      | Map.lookup q.id s.states == Just Dormant = Nothing
      | otherwise =
          Just
            (Map.restrictKeys (bestDesigns space p) (Set.fromList [c | c <- choices q, Map.lookup c s.verdicts == Just Possible]))
    consequencesOf space p = let f = compile space in bruteForceOn f (\q -> consequences q p)
    holds v (Take _) = v === Just Forced
    holds v (Refuse _) = v === Just Impossible

-- | Decide `next` from the prose: for each root option whose bounds
-- leave out some grade, the fewest options without a verified grade that,
-- graded the grade above `lo`, raise `lo`, and the fewest ungraded ones
-- that, graded below `hi`, lower `hi`; the cheaper, raising on a tie.
-- Each lead holds its option, move and how many it grades.
fewestGrades :: Space -> Set Literal -> [OptionId] -> [(OptionId, Move, Int)]
fewestGrades space p roots =
  [ (o, move, n)
  | o <- roots
  , let b = bestDesigns space p Map.! o
  , b.lo < b.hi
  , (move, n) <- take 1 (sortOn snd (catMaybes [(,) (Raise (b.lo + 1)) <$> raising o b, (,) (Lower b.hi) <$> lowering o b]))
  ]
  where
    options = [Listed x.id | q <- Map.elems space.questions, x <- q.options]
    assessment o = Map.lookup o assessed
    assessed = assessments space
    raising o b =
      fewest
        [c | c <- options, maybe True (.unverified) (assessment c)]
        (\cs -> (bestDesigns (gradedAt space (b.lo + 1) cs) p Map.! o).lo > b.lo)
    lowering o b =
      fewest
        [c | c <- options, isNothing (assessment c)]
        (\cs -> (bestDesigns (gradedAt space (b.hi - 1) cs) p Map.! o).hi < b.hi)
    fewest cs moves = listToMaybe [length s | s <- sortOn length (subsequences cs), moves s]

-- | Returns the possible listed options of root questions, by question and
-- option.
rootsOf :: Space -> Standing -> [OptionId]
rootsOf space s =
  [ o | q <- Map.elems space.questions, null q.raisedBy, o@(Listed _) <- choices q, Map.lookup o s.verdicts == Just Possible
  ]

assessments :: Space -> Map.Map OptionId Assessment
assessments space = Map.fromList [(Listed o.id, a) | q <- Map.elems space.questions, o <- q.options, Just a <- [o.assessment]]

-- | Holds when grading the lead's options as it says moves its bound.
leadMoves :: Space -> Set Literal -> Lead -> Property
leadMoves space p l = counterexample (show l) $ case l.move of
  Raise g -> (regraded g).lo > l.bounds.lo
  Lower g -> (regraded (g - 1)).hi < l.bounds.hi
  where
    regraded k = bestDesigns (gradedAt space k (Set.toList l.grading)) p Map.! l.option

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
    assessed = assessments space
    low o = maybe 0 (\a -> if a.unverified then 0 else position a.grade) (Map.lookup o assessed)
    high o = maybe top (position . (.grade)) (Map.lookup o assessed)
