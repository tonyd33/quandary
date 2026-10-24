module Frontends (frontends) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy qualified as LazyText
import Fixtures (fixture, listed, withCopy)
import Lucid (renderBS, renderText)
import Network.HTTP.Types (Status (..), decodePath, status200, status403, status404)
import Network.Wai (defaultRequest, pathInfo, requestHeaderHost, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Quandary.Core.Analyse (Analysis (..))
import Quandary.Core.Rulings (EditError (..))
import Quandary.Core.Types
import Quandary.Core.View (Objection (..), OptionRow (..), PointView (..), QuestionView (..), Whatif (..))
import Quandary.Shell.Browse
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
      , Points
      , WhatifAt "edit-offline" []
      , WhatifAt "current" [(AcceptVerb, "sync/git"), (DefeatVerb, "sync/how.other"), (ReopenVerb, "sync/lww"), (ReopenVerb, "sync/how")]
      ]
  ]
    ++ [ testCase "the root is the point served" $ uncurry (route "edit-offline") (decodePath "/") @?= Just (StatusAt "edit-offline")
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
          [idText o.raising.question.id | r <- v.rows, r.option == listed "sync/git", o <- r.objections] @?= ["conflict/shown", "sync/merge-conflicts"]
        _ -> assertFailure "not a question page"
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
      let out = LazyText.toStrict (renderText (markdown "Hi <script>alert(1)</script> there.\n\n<script>alert(2)</script>\n\n[x](javascript:alert(3))\n"))
      assertBool (Text.unpack out) (not ("<script" `Text.isInfixOf` out))
      assertBool (Text.unpack out) (not ("javascript:" `Text.isInfixOf` out))
  ]

server :: [TestTree]
server =
  [ testCase ("Host " <> show h <> " answers " <> show (statusCode expected)) $ statusFor h ["p", "current"] >>= (@?= expected)
  | (h, expected) <-
      [ (Just "127.0.0.1:8080", status200)
      , (Just "localhost:8080", status200)
      , (Just "localhost:9090", status403)
      , (Just "evil.example:8080", status403)
      , (Just "evil.example", status403)
      , (Nothing, status403)
      ]
  ]
    ++ [ testCase "an unknown question answers 404" $ statusFor (Just "localhost:8080") ["p", "current", "q", "sync", "nope"] >>= (@?= status404)
       , testCase "an unknown point answers 404" $ statusFor (Just "localhost:8080") ["p", "typo"] >>= (@?= status404)
       ]

statusFor :: Maybe Text.Text -> [Text.Text] -> IO Status
statusFor host path = withLive fixture $ \l -> do
  got <- newIORef Nothing
  let req = defaultRequest {requestHeaderHost = Text.encodeUtf8 <$> host, pathInfo = path}
  _ <- app l "current" 8080 req (\res -> ResponseReceived <$ writeIORef got (Just (responseStatus res)))
  readIORef got >>= maybe (assertFailure "no response") pure

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
