module Commands (commands) where

import Data.ByteString.Lazy qualified as Lazy
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Fixtures
  ( because
  , cleanModelOf
  , fixture
  , listed
  , modelAt
  , modelOf
  , option
  , other
  , question
  , rank
  , spaceOf
  , withCopy
  )
import Quandary.Core.Analyse
import Quandary.Core.Order (decisionOrder)
import Quandary.Core.Rulings
import Quandary.Core.Types hiding (rank)
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View (pointViews, questionViewFrom)
import Quandary.Shell.Render.Dot qualified as Dot
import Quandary.Shell.Render.Graph qualified as Graph
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
              { defeat =
                  Map.insert
                    (other "sync/how")
                    (Ruling Nothing (Just (because "all there is: #1, no")))
                    (pointNamed m.points "current").defeat
              , defer = Set.fromList [Id "sync/merge-conflicts"]
              , prefer = Map.fromList [(listed "sync/git", rank 8), (listed "conflict/diff", rank 0)]
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
          Map.lookup (Id "sync/how") s.states @?= Just (Open Raised)
  , testCase "an unraised question names the raisers still possible" $ do
      m <- model
      a <- analyse cadical m "current"
      fmap (Map.lookup (Id "conflict/shown") . (.states)) (standing a)
        @?= Just (Just (Open (Unraised ((listed "sync/git" :| []) :| []))))
  , testCase "an open question with an option taken" $ do
      let m = modelAt pickAny (Set.singleton (Take (listed "a")))
      a <- analyse cadical m "current"
      fmap (Map.lookup (Id "q") . (.states)) (standing a) @?= Just (Just (Open Taken))
  , testCase "an option taken without a raiser taken counts as taken" $ do
      a <- analyse cadical takenUnraised "current"
      fmap (Map.lookup (Id "q") . (.states)) (standing a) @?= Just (Just (Open Taken))
  , testCase "status lists a taken question under taken, not raised" $ do
      a <- analyse cadical takenUnraised "current"
      let shown = Text.lines (Render.status takenUnraised "current" a Map.empty Nothing)
      assertBool "taken, more possible" ("taken, more possible" `elem` shown)
      assertBool "raised if taken" ("raised if taken" `notElem` shown)
  , testCase "a question whose answer the rulings force is closed" $ do
      m <- model
      let m' =
            m
              { points =
                  Map.adjust
                    (\p -> p {defeat = Map.insert (other "sync/how") (Ruling Nothing (Just (because "all there is"))) p.defeat})
                    "current"
                    m.points
              }
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
          Map.lookup (Id "conflict/compare") s.states @?= Just (Open Raised)
          Map.lookup (listed "sync/git") s.verdicts @?= Just Forced
  ]

edits :: [TestTree]
edits =
  [ testCase "accepting into a stale point is refused" $ do
      m <- model
      r <- edit cadical m "current" [Rule (Accept (listed "sync/crdt") Nothing)]
      refusal r
        @?= Just (WouldBeStale [Set.fromList [Take (listed "store/files"), Take (listed "sync/crdt")]])
  , testCase "defeating into a stale point is refused" $ do
      m <- model
      let m' =
            m
              { points =
                  Map.adjust
                    ( \p -> p {defeat = Map.insert (listed "sync/git") (Ruling (Just "Git repo under the hood") (Just (because "no"))) p.defeat}
                    )
                    "current"
                    m.points
              }
      r <- edit cadical m' "current" [Rule (Defeat (other "sync/how") (because "all there is"))]
      refusal r
        @?= Just
          ( WouldBeStale
              [ Set.fromList
                  [Take (listed "store/files"), Refuse (listed "sync/git"), Refuse (listed "sync/lww"), Refuse (other "sync/how")]
              ]
          )
  , testCase "a point's own ruling replaces the inherited one" $ do
      m <- model
      let m' = m {points = Map.insert "branch" ((emptyPoint "branch") {extends = Just "current"}) m.points}
      edit cadical m' "branch" [Rule (Accept (listed "sync/lww") Nothing)] >>= single >>= \ed -> do
        let rs = rulingsOf ed.model.points "branch"
        (Set.member (Take (listed "sync/lww")) rs, Set.member (Refuse (listed "sync/lww")) rs) @?= (True, False)
  , testCase "reopening an inherited ruling drops it" $ do
      m <- model
      edit cadical m "edit-offline" [Rule (Reopen (listed "store/files"))] >>= single >>= \ed -> do
        (pointNamed ed.model.points "edit-offline").drop @?= Set.singleton (listed "store/files")
        Set.member (Take (listed "store/files")) (rulingsOf ed.model.points "edit-offline") @?= False
  , testCase "a ruling reports the points extending it that it leaves stale" $ do
      m <- model
      r <- edit cadical m "current" [Rule (Defeat (listed "store/files") (because "no"))]
      fmap (map (.moved.branches)) r
        @?= Right
          [
            [
              ( "edit-offline"
              , Just
                  [ Set.fromList [Take (listed "sync/git"), Refuse (listed "store/files")]
                  , Set.fromList [Take (listed "conflict/diff"), Refuse (listed "store/files"), Refuse (listed "sync/lww")]
                  ]
              )
            ]
          ]
  , testCase "why gives every set of rulings" $ do
      m <- model
      let rs = Set.insert (Take (listed "sync/git")) (rulingsOf m.points "current")
      cores <- whyAll cadical m.space rs (Take (listed "sync/crdt"))
      cores
        @?= [Set.singleton (Take (listed "store/files")), Set.singleton (Take (listed "sync/git"))]
  , testCase "a proposed branch is adopted only once confirmed" $ do
      m <- model
      let m' = m {points = Map.insert "idea" ((emptyPoint "idea") {extends = Just "current", proposed = True}) m.points}
      unconfirmed <- adopt cadical m' "idea" Unconfirmed
      either Just (const Nothing) unconfirmed @?= Just Proposed
      confirmed <- adopt cadical m' "idea" Confirmed
      fmap (.into) confirmed @?= Right "current"
  , testCase "reopening what has no ruling" $ do
      m <- model
      r <- edit cadical m "current" [Rule (Reopen (listed "sync/git"))]
      refusal r
        @?= Just (Refused (NotRuled (listed "sync/git")))
  , testCase "accepting records the title and reports what moved" $ do
      m <- model
      edit cadical m "current" [Rule (Accept (listed "sync/git") (Just (because "history for free")))] >>= single >>= \ed -> do
        Map.lookup (listed "sync/git") (pointNamed ed.model.points "current").accept
          @?= Just (Ruling (Just "Git repo under the hood") (Just (because "history for free")))
        ed.moved.options @?= [(listed "sync/git", Possible, Forced), (other "sync/how", Possible, Impossible)]
        ed.moved.questions
          @?= [ (Id "conflict/shown", Open (Unraised ((listed "sync/git" :| []) :| [])), Open Raised)
              , (Id "sync/how", Open Raised, Closed)
              , (Id "sync/merge-conflicts", Open (Unraised ((listed "sync/git" :| []) :| [])), Open Raised)
              ]
  , testCase "settling defeats the options still possible" $ do
      r <- edit cadical (modelAt pickAny Set.empty) "current" [Settle (pickAny.questions Map.! Id "q") (because "enough")]
      fmap (map (.applied)) r @?= Right [[Defeat o (because "enough") | o <- [listed "a", listed "b", other "q"]]]
  , testCase "a settle after an edit settles what that edit leaves possible" $ do
      r <-
        edit
          cadical
          (modelAt pickAny Set.empty)
          "current"
          [Rule (Accept (listed "a") Nothing), Settle (pickAny.questions Map.! Id "q") (because "enough")]
      fmap (map (.applied)) r
        @?= Right [[Accept (listed "a") Nothing], [Defeat o (because "enough") | o <- [listed "b", other "q"]]]
  , testCase "settling a pick-one question with nothing taken is refused" $ do
      m <- model
      r <- edit cadical m "current" [Settle (m.space.questions Map.! Id "sync/how") (because "all of it")]
      refusal r
        @?= Just
          ( WouldBeStale
              [ Set.fromList
                  [Take (listed "store/files"), Refuse (listed "sync/git"), Refuse (listed "sync/lww"), Refuse (other "sync/how")]
              ]
          )
  ]

single :: Either (Model, Refusal) [Step] -> IO Step
single = \case
  Right [ed] -> pure ed
  r -> assertFailure (show r)

refusal :: Either (Model, Refusal) a -> Maybe Refusal
refusal = either (Just . snd) (const Nothing)

takenUnraised :: Model
takenUnraised = modelAt space (Set.fromList [Refuse (other "r"), Refuse (other "q")])
  where
    space =
      spaceOf
        [ question "r" One [] [option "a", option "b"]
        , question "q" Some [listed "a" :| [], listed "b" :| []] [option "x", (option "y") {needs = [listed "x"]}]
        ]

pickAny :: Space
pickAny = spaceOf [question "q" Any [] [option "a", option "b"]]

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
      pure (Render.status m "current" a Map.empty (Just (pointViews m staleNames)))
  , golden "show" $ \m -> do
      a <- analyse cadical m "current"
      pure (Render.showQuestion m.space (questionViewFrom m "current" a Map.empty (m.space.questions Map.! Id "sync/how")))
  , golden "why" $ \m -> do
      cores <- whyAll cadical m.space (rulingsOf m.points "current") (Take (listed "sync/crdt"))
      Render.why m "current" (listed "sync/crdt") Impossible <$> explain cadical m.space (listed "sync/crdt") Impossible cores
  , golden "graph" $ \m -> Mermaid.graph <$> spaceGraph m
  , golden "graph-dot" $ \m -> Dot.graph <$> spaceGraph m
  , testCase "dot joins options raising together and draws forbid entries" $ do
      let base = question "a" One [] [option "x", option "y"]
          raised = question "b" One [listed "x" :| [listed "y"]] [option "z"]
          space = spaceOf [base {forbid = [Forbid (Set.fromList [Take (listed "x"), Refuse (listed "z")]) (Just "no")]}, raised]
      Text.lines (Dot.graph (Graph.build space Nothing))
        @?= [ "digraph space {"
            , "  rankdir=LR"
            , "  compound=true"
            , "  node [shape=box, style=rounded]"
            , "  subgraph \"cluster_a\" {"
            , "    label=\"a: \""
            , "    \"x\" [label=\"x: \"]"
            , "    \"y\" [label=\"y: \"]"
            , "    \"a.other\" [label=\"a.other\"]"
            , "  }"
            , "  subgraph \"cluster_b\" {"
            , "    label=\"b: \""
            , "    \"z\" [label=\"z: \"]"
            , "    \"b.other\" [label=\"b.other\"]"
            , "  }"
            , "  \"joint b 0\" [shape=circle, label=\"all of\"]"
            , "  \"joint b 0\" -> \"b.other\" [lhead=\"cluster_b\", penwidth=2]"
            , "  \"x\" -> \"joint b 0\" [dir=none]"
            , "  \"y\" -> \"joint b 0\" [dir=none]"
            , "  \"forbid 0\" [shape=hexagon, label=\"forbid: no\"]"
            , "  \"forbid 0\" -> \"x\" [style=dotted, dir=none, label=\"with\"]"
            , "  \"forbid 0\" -> \"z\" [style=dotted, dir=none, label=\"without\"]"
            , "}"
            ]
  ]
  where
    spaceGraph m = do
      a <- analyse cadical m "current"
      pure (Graph.build m.space (standing a))
    golden name render =
      goldenVsString name ("test/golden/" <> name <> ".txt") (Lazy.fromStrict . Text.encodeUtf8 <$> (model >>= render))
