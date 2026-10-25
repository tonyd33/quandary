module Frontends (frontends) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (evaluate)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy qualified as LazyText
import Fixtures (fixture, listed, withCopy)
import Lucid (renderBS, renderText)
import Network.HTTP.Types (Status (..), decodePath, hLocation, parseQuery, status200, status303, status403, status404)
import Network.Wai (defaultRequest, pathInfo, queryString, requestHeaderHost, responseHeaders, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Quandary.Core.Analyse (Analysis (..))
import Quandary.Core.Rulings (EditError (..))
import Quandary.Core.Types
import Quandary.Core.View
  ( Objection (..)
  , OptionRow (..)
  , OptionView (..)
  , PointView (..)
  , QuestionView (..)
  , Whatif (..)
  )
import Quandary.Shell.Browse
import Quandary.Shell.Fuzzy (ranked, score)
import Quandary.Shell.Render.Html (document, link, markdown)
import Quandary.Shell.Resolve (Verb (..))
import Quandary.Shell.Serve (app, route)
import Quandary.Shell.Storage.Files (FileProblem (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

frontends :: TestTree
frontends =
  testGroup
    "frontends"
    [ testGroup "routes" routes
    , testGroup "pages" pages
    , testGroup "html" html
    , testGroup "go to" goTo
    , testGroup "server" server
    , testGroup "live" live
    ]

routes :: [TestTree]
routes =
  [ testCase ("link then route gives back " <> show r) $ parsed (link r) @?= Just r
  | r <-
      [ StatusAt "current"
      , StatusAt "a/b"
      , QuestionAt "current" (Id "sync/how")
      , OptionAt "current" "sync/git"
      , OptionAt "current" "sync/how.other"
      , Points
      , GoTo "current" "pseqrp"
      , WhatifAt "edit-offline" []
      , WhatifAt
          "current"
          [(AcceptVerb, "sync/git"), (DefeatVerb, "sync/how.other"), (ReopenVerb, "sync/lww"), (ReopenVerb, "sync/how")]
      ]
  ]
    ++ [ testCase "the root is the point served" $
           uncurry (route "edit-offline") (decodePath "/") @?= Just (StatusAt "edit-offline")
       , testCase "an unknown verb is no route" $ parsed "/p/current/whatif?take=sync/git" @?= Nothing
       ]

parsed :: Text.Text -> Maybe Route
parsed = uncurry (route "current") . decodePath . Text.encodeUtf8

drawnAt :: Route -> IO Drawn
drawnAt r = withLive fixture (`page` r)

pageAt :: Route -> IO Page
pageAt r =
  drawnAt r >>= \d -> case d.shown of
    Shown _ p -> pure p
    Problems ps -> assertFailure ("problems: " <> show ps)

pages :: [TestTree]
pages =
  [ testCase "the status page at current is fresh with no stale points" $
      pageAt (StatusAt "current") >>= \case
        StatusPage "current" (Fresh _) _ stale -> null stale @? "no stale points"
        _ -> assertFailure "not a fresh status page"
  , testCase "sync/git's objections are the questions it raises" $
      pageAt (QuestionAt "current" (Id "sync/how")) >>= \case
        QuestionPage v -> do
          v.question.id @?= Id "sync/how"
          [idText o.raising.question.id | r <- v.rows, r.option == listed "sync/git", o <- r.objections]
            @?= ["conflict/shown", "sync/merge-conflicts"]
        _ -> assertFailure "not a question page"
  , testCase "an impossible option's page gives every set of rulings responsible" $
      pageAt (OptionAt "current" "sync/crdt") >>= \case
        OptionPage v -> do
          (v.question.id, v.row.verdict) @?= (Id "sync/how", Just Impossible)
          v.causes @?= [Set.singleton (Take (listed "store/files"))]
        _ -> assertFailure "not an option page"
  , testCase "an option defeated for no other cause has no causes" $
      pageAt (OptionAt "current" "sync/lww") >>= \case
        OptionPage v -> (v.row.verdict, v.causes) @?= (Just Impossible, [])
        _ -> assertFailure "not an option page"
  , testCase "a possible option's page gives the questions it raises and no causes" $
      pageAt (OptionAt "current" "sync/git") >>= \case
        OptionPage v -> do
          [idText o.raising.question.id | o <- v.row.objections] @?= ["conflict/shown", "sync/merge-conflicts"]
          v.causes @?= []
        _ -> assertFailure "not an option page"
  , testCase "a question's id is no option" $
      pageAt (OptionAt "current" "sync/how") >>= \case
        NotFound t -> t @?= "unknown option sync/how"
        _ -> assertFailure "found"
  , testCase "the points page lists every point, none stale" $
      pageAt Points >>= \case
        PointsPage ps -> [(pv.point.name, pv.stale) | pv <- ps] @?= [("current", False), ("edit-offline", False)]
        _ -> assertFailure "not the points"
  , testCase "accepting sync/git raises questions" $
      pageAt (WhatifAt "current" [(AcceptVerb, "sync/git")]) >>= \case
        WhatifPage "current" _ (Right w) -> not (null w.implications) @? "accepting sync/git raises questions"
        _ -> assertFailure "not a whatif page"
  , testCase "an unknown question is not found" $
      pageAt (QuestionAt "current" (Id "sync/nope")) >>= \case
        NotFound t -> t @?= "unknown question sync/nope"
        _ -> assertFailure "found"
  , testCase "a whatif on an unknown option is not found" $
      pageAt (WhatifAt "current" [(AcceptVerb, "sync/nope")]) >>= \case
        NotFound t -> t @?= "unknown id sync/nope"
        _ -> assertFailure "found"
  , testCase "a whatif reopening a question not deferred is refused" $
      pageAt (WhatifAt "current" [(ReopenVerb, "sync/how")]) >>= \case
        WhatifPage "current" _ (Left e) -> e @?= NotDeferred (Id "sync/how")
        _ -> assertFailure "not a refused whatif"
  , testCase "a fresh point lands on the first question it lists" $
      (landing True <$> drawnAt (StatusAt "current")) >>= (@?= Just (QuestionAt "current" (Id "sync/how")))
  , testCase "a question page lands nowhere" $
      (landing True <$> drawnAt (QuestionAt "current" (Id "sync/how"))) >>= (@?= Nothing)
  , testCase "a stale point lands nowhere" $ withCopy $ \dir -> do
      let file = dir </> "points/current.md"
      TextIO.readFile file
        >>= TextIO.writeFile file . Text.replace "accept:\n" "accept:\n  sync/crdt: {title: CRDT per note}\n"
      d <- withLive dir (`page` StatusAt "current")
      landing True d @?= Nothing
  , testCase "an unknown point is not found" $
      pageAt (StatusAt "typo") >>= \case
        NotFound t -> t @?= "unknown point typo"
        _ -> assertFailure "found"
  ]

html :: [TestTree]
html =
  [ goldenVsString "question page" "test/golden/question.html" $ do
      let r = QuestionAt "current" (Id "sync/how")
      renderBS . document r <$> drawnAt r
  , testCase "a body's raw HTML is dropped" $ do
      let out =
            LazyText.toStrict
              (renderText (markdown "Hi <script>alert(1)</script> there.\n\n<script>alert(2)</script>\n\n[x](javascript:alert(3))\n"))
      assertBool (Text.unpack out) (not ("<script" `Text.isInfixOf` out))
      assertBool (Text.unpack out) (not ("javascript:" `Text.isInfixOf` out))
  , testCase "bodies render alike when rendered concurrently" $ do
      let body i = "| n | x |\n|---|---|\n| " <> Text.pack (show i) <> " | y |\n\n~~gone " <> Text.pack (show i) <> "~~\n"
      outs <- forConcurrently [1 .. 64 :: Int] $ \i -> evaluate (LazyText.toStrict (renderText (markdown (body i))))
      sequence_
        [ assertBool (Text.unpack out) (all (`Text.isInfixOf` out) ["<td>" <> n <> "</td>", "<del>gone " <> n <> "</del>"])
        | (i, out) <- zip [1 :: Int ..] outs
        , let n = Text.pack (show i)
        ]
  , testCase "a body renders GitHub-flavoured tables, strikethrough and autolinks" $ do
      let out = LazyText.toStrict (renderText (markdown "| a | b |\n|---|---|\n| 1 | 2 |\n\n~~gone~~ https://example.org\n"))
      assertBool (Text.unpack out) ("<table>" `Text.isInfixOf` out && "<td>1</td>" `Text.isInfixOf` out)
      assertBool (Text.unpack out) ("<del>gone</del>" `Text.isInfixOf` out)
      assertBool (Text.unpack out) ("<a href=\"https://example.org\">" `Text.isInfixOf` out)
  ]

server :: [TestTree]
server =
  [ testCase ("Host " <> show h <> " answers " <> show (statusCode expected)) $
      statusFor h ["p", "current"] >>= (@?= expected)
  | (h, expected) <-
      [ (Just "127.0.0.1:8080", status200)
      , (Just "localhost:8080", status200)
      , (Just "localhost:9090", status403)
      , (Just "evil.example:8080", status403)
      , (Just "evil.example", status403)
      , (Nothing, status403)
      ]
  ]
    ++ [ testCase "an unknown question answers 404" $
           statusFor (Just "localhost:8080") ["p", "current", "q", "sync", "nope"] >>= (@?= status404)
       , testCase "an unknown point answers 404" $ statusFor (Just "localhost:8080") ["p", "typo"] >>= (@?= status404)
       ]

statusFor :: Maybe Text.Text -> [Text.Text] -> IO Status
statusFor host path = fst <$> responseFor host path ""

-- | Returns the status and the `Location` header answering the request.
responseFor :: Maybe Text.Text -> [Text.Text] -> Text.Text -> IO (Status, Maybe Text.Text)
responseFor host path q = withLive fixture $ \l -> do
  got <- newIORef Nothing
  let req =
        defaultRequest
          { requestHeaderHost = Text.encodeUtf8 <$> host
          , pathInfo = path
          , queryString = parseQuery (Text.encodeUtf8 q)
          }
      seen res = (responseStatus res, Text.decodeUtf8 <$> lookup hLocation (responseHeaders res))
  _ <- app l "current" 8080 req (\res -> ResponseReceived <$ writeIORef got (Just (seen res)))
  readIORef got >>= maybe (assertFailure "no response") pure

goTo :: [TestTree]
goTo =
  [ testCase "pseqrp finds pattern/sequence/run-parsers first" $
      map fst (ranked "pseqrp" (map (\t -> (t, ())) ids))
        @?= ["pattern/sequence/run-parsers", "pattern/sequence/run-views/prefix-view", "pattern/sequence/run-views/names/prelude"]
  , testCase "a query out of order matches nothing" $ score "srp" "pattern/sequence" @?= Nothing
  , testCase "case is ignored" $ assertBool "no match" (isJust (score "SH" "sync/how"))
  , testCase "a match at a segment start beats one inside a word" $
      map fst (ranked "sh" [("sash", ()), ("sync/how", ())]) @?= ["sync/how", "sash"]
  , testCase "consecutive characters beat scattered ones" $
      map fst (ranked "ab" [("xaxb", ()), ("xab", ())]) @?= ["xab", "xaxb"]
  , testCase "shorter ids go first on ties" $
      map fst (ranked "sync/how" [("sync/how.other", ()), ("sync/how", ())]) @?= ["sync/how", "sync/how.other"]
  , testCase "an empty query lists every id, shorter first" $
      map fst (ranked "" [("bb", ()), ("a", ())]) @?= ["a", "bb"]
  , testCase "go redirects to the best match's question" $
      responseFor (Just "localhost:8080") ["p", "current", "go"] "?id=synchow"
        >>= (@?= (status303, Just "/p/current/q/sync/how"))
  , testCase "go redirects to the best match's option" $
      responseFor (Just "localhost:8080") ["p", "edit-offline", "go"] "?id=lww"
        >>= (@?= (status303, Just "/p/edit-offline/o/sync/lww"))
  , testCase "go to an id nothing matches answers 404" $
      responseFor (Just "localhost:8080") ["p", "current", "go"] "?id=zzz" >>= (@?= (status404, Nothing))
  ]
  where
    ids =
      ["pattern/sequence/run-views/prefix-view", "pattern/sequence/run-views/names/prelude", "pattern/sequence/run-parsers"]

live :: [TestTree]
live =
  [ testCase "writing a question file bumps the revision" $ withCopy $ \dir -> withLive dir $ \l -> do
      r0 <- currentRevision l
      TextIO.appendFile (dir </> "questions/sync/how.md") "\nMore prose.\n"
      changed <- timeout 5000000 (awaitChange l r0)
      assertBool "no change" (maybe False (/= r0) changed)
  , testCase "rewriting a file unchanged leaves the revision" $ withCopy $ \dir -> withLive dir $ \l -> do
      r0 <- currentRevision l
      TextIO.readFile (dir </> "questions/sync/how.md") >>= TextIO.writeFile (dir </> "questions/sync/how.md")
      changed <- timeout 1000000 (awaitChange l r0)
      changed @?= Nothing
  , testCase "a file outside the space files leaves the revision" $ withCopy $ \dir -> withLive dir $ \l -> do
      r0 <- currentRevision l
      createDirectoryIfMissing True (dir </> ".jj")
      TextIO.writeFile (dir </> ".jj/working.md") "x"
      TextIO.writeFile (dir </> "questions/scratch.txt") "x"
      changed <- timeout 1000000 (awaitChange l r0)
      changed @?= Nothing
  , testCase "a broken file shows its problems" $ withCopy $ \dir -> withLive dir $ \l -> do
      r0 <- currentRevision l
      TextIO.writeFile (dir </> "questions/sync/how.md") "no frontmatter\n"
      _ <- timeout 5000000 (awaitChange l r0)
      d <- page l (StatusAt "current")
      case d.shown of
        Problems ps -> assertBool (show ps) (("questions/sync/how.md" :: FilePath) `elem` [p.file | p <- ps])
        Shown _ _ -> assertFailure "shown"
  ]
