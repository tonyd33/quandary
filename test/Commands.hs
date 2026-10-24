module Commands (commands) where

import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Fixtures (cleanModelOf, fixture, listed, modelOf, other, withCopy)
import Quandary.Core.Analyse
import Quandary.Core.Order (decisionOrder)
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Shell.Render.Mermaid qualified as Mermaid
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical (cadical)
import Quandary.Shell.Storage.Files
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

commands :: TestTree
commands =
  testGroup
    "commands"
    [ testGroup "points" points
    , testGroup "states" states
    , testGroup "edits" edits
    , testGroup "order" order
    , testGroup "rendering" rendering
    ]

model :: IO Model
model = modelOf fixture

points :: [TestTree]
points =
  [ testCase "a written point reads back the same" $ withCopy $ \dir -> do
      m <- cleanModelOf dir
      let p =
            (pointNamed m.points "current")
              { defeat = Map.insert (other "sync/how") (Ruling Nothing (Just "all there is: #1, no")) (pointNamed m.points "current").defeat
              , defer = Set.fromList [Id "sync/merge-conflicts"]
              , prefer = Map.fromList [(listed "sync/git", 8), (listed "conflict/diff", 0)]
              , body = "Prose.\n"
              }
      writePoint dir p
      m' <- cleanModelOf dir
      Map.lookup "current" m'.points @?= Just p
  ]

states :: [TestTree]
states =
  [ testCase "at current" $ do
      m <- model
      analyse cadical m "current" >>= \case
        StaleAt _ -> assertFailure "stale"
        Fresh s -> do
          Map.lookup (Id "store/where") s.states @?= Just Closed
          Map.lookup (Id "store/editors") s.states @?= Just Dormant
          Map.lookup (Id "sync/how") s.states @?= Just Open
  , testCase "a question whose answer the rulings force is closed" $ do
      m <- model
      let m' = m {points = Map.adjust (\p -> p {defeat = Map.insert (other "sync/how") (Ruling Nothing (Just "all there is")) p.defeat}) "current" m.points}
      a <- analyse cadical m' "current"
      fmap (Map.lookup (Id "sync/how") . (.states)) (standing a) @?= Just (Just Closed)
  , testCase "deferred" $ do
      m <- model
      let m' = m {points = Map.adjust (\p -> p {defer = Set.singleton (Id "sync/how")}) "current" m.points}
      a <- analyse cadical m' "current"
      fmap (Map.lookup (Id "sync/how") . (.states)) (standing a) @?= Just (Just Deferred)
  , testCase "a branch reaches its raiser's answers" $ do
      m <- model
      analyse cadical m "edit-offline" >>= \case
        StaleAt _ -> assertFailure "stale"
        Fresh s -> do
          Map.lookup (Id "conflict/compare") s.states @?= Just Open
          Map.lookup (listed "sync/git") s.verdicts @?= Just Forced
  ]

edits :: [TestTree]
edits =
  [ testCase "accepting into a stale point is refused" $ do
      m <- model
      r <- edit cadical m "current" [Accept (listed "sync/crdt") Nothing]
      either Just (const Nothing) r
        @?= Just (WouldBeStale [Set.fromList [Take (listed "store/files"), Take (listed "sync/crdt")]])
  , testCase "defeating into a stale point is refused" $ do
      m <- model
      let m' = m {points = Map.adjust (\p -> p {defeat = Map.insert (listed "sync/git") (Ruling (Just "Git repo under the hood") (Just "no")) p.defeat}) "current" m.points}
      r <- edit cadical m' "current" [Defeat (other "sync/how") "all there is"]
      either Just (const Nothing) r
        @?= Just (WouldBeStale [Set.fromList [Take (listed "store/files"), Refuse (listed "sync/git"), Refuse (listed "sync/lww"), Refuse (other "sync/how")]])
  , testCase "a point's own ruling replaces the inherited one" $ do
      m <- model
      let m' = m {points = Map.insert "branch" ((pointNamed Map.empty "branch") {extends = Just "current"}) m.points}
      edit cadical m' "branch" [Accept (listed "sync/lww") Nothing] >>= \case
        Left r -> assertFailure (show r)
        Right (m'', _) -> do
          let rs = rulingsOf m''.points "branch"
          (Set.member (Take (listed "sync/lww")) rs, Set.member (Refuse (listed "sync/lww")) rs) @?= (True, False)
  , testCase "reopening an inherited ruling drops it" $ do
      m <- model
      edit cadical m "edit-offline" [Reopen (listed "store/files")] >>= \case
        Left r -> assertFailure (show r)
        Right (m', _) -> do
          (pointNamed m'.points "edit-offline").drop @?= Set.singleton (listed "store/files")
          Set.member (Take (listed "store/files")) (rulingsOf m'.points "edit-offline") @?= False
  , testCase "a ruling reports the points extending it that it leaves stale" $ do
      m <- model
      r <- edit cadical m "current" [Defeat (listed "store/files") "no"]
      fmap ((.branches) . snd) r
        @?= Right [("edit-offline", Just [Set.fromList [Take (listed "sync/git"), Refuse (listed "store/files")], Set.fromList [Take (listed "conflict/diff"), Refuse (listed "store/files"), Refuse (listed "sync/lww")]])]
  , testCase "why gives every set of rulings" $ do
      m <- model
      let rs = Set.insert (Take (listed "sync/git")) (rulingsOf m.points "current")
      cores <- whyAll cadical m.space rs (Take (listed "sync/crdt"))
      cores
        @?= [Set.singleton (Take (listed "store/files")), Set.singleton (Take (listed "sync/git"))]
  , testCase "reopening what has no ruling" $ do
      m <- model
      r <- edit cadical m "current" [Reopen (listed "sync/git")]
      either Just (const Nothing) r
        @?= Just (Refused (NotRuled (listed "sync/git")))
  , testCase "accepting records the title and reports what moved" $ do
      m <- model
      edit cadical m "current" [Accept (listed "sync/git") (Just "history for free")] >>= \case
        Left r -> assertFailure (show r)
        Right (m', mv) -> do
          Map.lookup (listed "sync/git") (pointNamed m'.points "current").accept @?= Just (Ruling (Just "Git repo under the hood") (Just "history for free"))
          mv.options @?= [(listed "sync/git", Possible, Forced), (other "sync/how", Possible, Impossible)]
          mv.questions @?= [(Id "sync/how", Open, Closed)]
  ]

order :: [TestTree]
order =
  [ testCase "a raised question follows its raiser's question" $ do
      m <- model
      let open = Set.fromList (map Id ["sync/how", "conflict/shown", "conflict/compare"])
      decisionOrder m.space open @?= [[Id "sync/how"], [Id "conflict/shown"], [Id "conflict/compare"]]
  , testCase "the question touched by more constraints goes first" $ do
      m <- model
      let open = Set.fromList (map Id ["conflict/compare", "conflict/phone", "conflict/when", "sync/how"])
      decisionOrder m.space open @?= [[Id "sync/how", Id "conflict/compare"], [Id "conflict/phone", Id "conflict/when"]]
  ]

rendering :: [TestTree]
rendering =
  [ golden "status" $ \m -> do
      a <- analyse cadical m "current"
      staleNames <- everyStale cadical m
      pure (Render.status m "current" a Map.empty (Just staleNames))
  , golden "show" $ \m -> do
      a <- analyse cadical m "current"
      pure (Render.showQuestion m "current" a Map.empty (m.space.questions Map.! Id "sync/how"))
  , golden "why" $ \m -> Render.why m "current" (listed "sync/crdt") Impossible <$> whyAll cadical m.space (rulingsOf m.points "current") (Take (listed "sync/crdt"))
  , golden "graph" $ \m -> do
      a <- analyse cadical m "current"
      pure (Mermaid.graph m.space ((.verdicts) <$> standing a))
  ]
  where
    golden name render =
      goldenVsString name ("test/golden/" <> name <> ".txt") (Lazy.fromStrict . Text.encodeUtf8 <$> (model >>= render))
