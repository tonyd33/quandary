module Storage (storage) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Fixtures (because, fixture, modelOf)
import Quandary.Core.Compile (compile)
import Quandary.Core.Rulings (rulingsOf)
import Quandary.Core.Solver
import Quandary.Core.Source
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Solver.Cadical (cadical)
import Quandary.Shell.Storage.Files hiding (problemsOf)
import Quandary.Shell.Storage.Files qualified as Files
import Quandary.Shell.Storage.Yaml (YamlIssue (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

storage :: TestTree
storage =
  testGroup
    "storage"
    [ testGroup "notes-sync fixture" notesSync
    , testGroup "format problems" formatProblems
    , testGroup "space problems" spaceProblems
    , testGroup "point problems" pointProblems
    , testGroup "grades" grades
    , testGroup "bodies" bodies
    ]

notesSync :: [TestTree]
notesSync =
  [ testCase "has no problems" $ do
      (problems, _) <- load fixture
      problems @?= []
  , testCase "reads raised-by, needs and option ids" $ do
      m <- modelOf fixture
      let q = m.space.questions Map.! Id "conflict/compare"
      q.raisedBy @?= [Listed (Id "conflict/diff") :| []]
      map (.id) q.options @?= [Id "conflict/two-way", Id "conflict/three-way"]
      [o.needs | o <- q.options] @?= [[], [Listed (Id "sync/git")]]
  , testCase "reads rulings with their titles" $ do
      m <- modelOf fixture
      let p = m.points Map.! "current"
      Map.keys p.accept @?= [Listed (Id "store/files")]
      (p.accept Map.! Listed (Id "store/files")).reason @?= Just (because "must open in any editor")
      (m.points Map.! "edit-offline").extends @?= Just "current"
  , testCase "current rules out the database" $ do
      m <- modelOf fixture
      let rulings = rulingsOf m.points "current"
          f = compile m.space
      (case cadical of Solver {solving} -> solving f (\q -> consequences q rulings)) >>= \case
        Stale -> assertFailure "stale"
        Reaches vs -> do
          Map.lookup (Listed (Id "store/sqlite")) vs @?= Just Impossible
          Map.lookup (Listed (Id "sync/crdt")) vs @?= Just Impossible
          Map.lookup (Listed (Id "sync/git")) vs @?= Just Possible
  ]

formatProblems :: [TestTree]
formatProblems =
  [ testCase "an unknown key" $
      problemsOf [question "q" ["title: Q", "pick: one", "rank: 3"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Format (UnknownKey "rank"))])
  , testCase "a duplicate key" $
      problemsOf [question "q" ["title: Q", "pick: one", "pick: any"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Format (Yaml (DuplicateKey "pick")))])
  , testCase "an anchor and an alias" $
      problemsOf [question "q" ["title: &t Q", "pick: one", "links: [{rel: gains, to: *t}]"] ["Q."]]
        >>= ( @?=
                [ ("questions/q.md", Just 2, Format (Yaml Anchor))
                , ("questions/q.md", Just 4, Format (Yaml Alias))
                , ("questions/q.md", Just 4, Semantic (UnknownTarget ""))
                ]
            )
  , testCase "a tag" $
      problemsOf [question "q" ["title: !!str Q", "pick: one"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 2, Format (Yaml Tag))])
  , testCase "a list where text belongs" $
      problemsOf [question "q" ["title: [Q]", "pick: one"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 2, Format (Expected TextShape))])
  , testCase "text where a list belongs" $
      problemsOf [question "q" ["title: Q", "pick: one", "raised-by: q"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Format (Expected ListShape))])
  , testCase "no frontmatter" $
      problemsOf [("questions/q.md", "Q.\n")]
        >>= ( @?=
                [ ("questions/q.md", Just 1, Format NoFrontmatter)
                , ("questions/q.md", Nothing, Semantic (Missing TitleField))
                , ("questions/q.md", Nothing, Semantic (Missing PickField))
                ]
            )
  , testCase "unclosed frontmatter" $
      problemsOf [("questions/q.md", "---\ntitle: Q\n")]
        >>= ( @?=
                [ ("questions/q.md", Just 1, Format UnclosedFrontmatter)
                , ("questions/q.md", Nothing, Semantic (Missing TitleField))
                , ("questions/q.md", Nothing, Semantic (Missing PickField))
                , ("questions/q.md", Nothing, Semantic MissingDescription)
                ]
            )
  , testCase "a YAML syntax error" $ do
      ps <- problemsOf [question "q" ["title: Q", "pick: [one"] ["Q."]]
      [(f, isSyntax i) | (f, _, i) <- take 1 ps] @?= [("questions/q.md", True)]
  ]
  where
    isSyntax (Format (Yaml (Syntax _))) = True
    isSyntax _ = False

spaceProblems :: [TestTree]
spaceProblems =
  [ testCase "every scalar is text" $
      problemsOf [question "q" ["title: Q", "pick: no"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 3, Semantic (BadPick "no"))])
  , testCase "a missing title" $
      problemsOf [question "q" ["pick: one"] ["Q."]]
        >>= (@?= [("questions/q.md", Nothing, Semantic (Missing TitleField))])
  , testCase "a missing option title" $
      problemsOf [question "q" ["title: Q", "pick: one", "options:", "  a: {}"] ["Q.", "", "## a", "", "A."]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (Missing TitleField))])
  , testCase "a missing description" $
      problemsOf [question "q" ["title: Q", "pick: one"] []]
        >>= (@?= [("questions/q.md", Nothing, Semantic MissingDescription)])
  , testCase "a missing option section" $
      problemsOf [question "q" ["title: Q", "pick: one", "options:", "  a: {title: A}"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (MissingSection "a"))])
  , testCase "an empty option section" $
      problemsOf
        [ question
            "q"
            ["title: Q", "pick: one", "options:", "  a: {title: A}"]
            ["Q.", "", "## a", "", "## Background", "", "More."]
        ]
        >>= (@?= [("questions/q.md", Just 9, Semantic MissingDescription)])
  , testCase "a duplicate option section" $
      problemsOf
        [ question
            "q"
            ["title: Q", "pick: one", "options:", "  a: {title: A}"]
            ["Q.", "", "## a", "", "A.", "", "## a", "", "A again."]
        ]
        >>= (@?= [("questions/q.md", Just 13, Semantic (DuplicateSection "a"))])
  , testCase "an unknown option" $
      problemsOf
        [question "q" ["title: Q", "pick: one", "options:", "  a: {title: A, needs: [b]}"] ["Q.", "", "## a", "", "A."]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (UnknownOption "b"))])
  , testCase "a raiser of options taken together" $ do
      (ps, m) <-
        loadFiles
          [ question
              "q"
              ["title: Q", "pick: any", "options:", "  a: {title: A}", "  b: {title: B}"]
              ["Q.", "", "## a", "", "A.", "", "## b", "", "B."]
          , question "r" ["title: R", "pick: one", "raised-by: [[a, b], q.other]"] ["R."]
          ]
      ps @?= []
      [(m'.space.questions Map.! Id "r").raisedBy | Just m' <- [m]]
        @?= [[Listed (Id "a") :| [Listed (Id "b")], Other (Id "q") :| []]]
  , testCase "an empty raiser" $
      problemsOf [question "r" ["title: R", "pick: one", "raised-by: [[]]"] ["R."]]
        >>= (@?= [("questions/r.md", Just 4, Semantic (EmptyList RaisedByField))])
  , testCase "an option of a topic directory" $
      problemsOf
        [ question "keys/where" ["title: Q", "pick: one", "options:", "  local: {title: A}"] ["Q.", "", "## keys/local", "", "A."]
        , question "keys/pairing" ["title: P", "pick: one", "raised-by: [keys/local, keys/where.other]"] ["P."]
        ]
        >>= (@?= [])
  , testCase "a question file named after its directory" $
      problemsOf
        [ question
            "pay/crash/crash"
            ["title: Q", "pick: any", "options:", "  retry: {title: A}"]
            ["Q.", "", "## pay/crash/retry", "", "A."]
        , question "pay/after" ["title: P", "pick: one", "raised-by: [pay/crash/retry, pay/crash.other]"] ["P."]
        ]
        >>= (@?= [])
  , testCase "a question named after its directory collides with its sibling file" $
      problemsOf
        [ question "a/b/b" ["title: X", "pick: one"] ["X."]
        , question "a/b" ["title: Y", "pick: one"] ["Y."]
        ]
        >>= (@?= [("questions/a/b.md", Nothing, Semantic (Collision "a/b" (Loc (QuestionDoc "a/b/b") Nothing)))])
  , testCase "ids collide ignoring case" $
      problemsOf
        [ question "a/x" ["title: X", "pick: one", "options:", "  y: {title: Y}"] ["X.", "", "## a/y", "", "Y."]
        , question "a/Y" ["title: Y", "pick: one"] ["Y."]
        ]
        >>= (@?= [("questions/a/x.md", Just 5, Semantic (Collision "a/y" (Loc (QuestionDoc "a/Y") Nothing)))])
  , testCase "an option key spelled other" $
      problemsOf [question "q" ["title: Q", "pick: one", "options:", "  other: {title: O}"] ["Q.", "", "## other", "", "O."]]
        >>= (@?= [("questions/q.md", Just 5, Semantic ReservedKey)])
  , testCase "a bad id" $
      problemsOf [question "a b" ["title: Q", "pick: one"] ["Q."]]
        >>= (@?= [("questions/a b.md", Nothing, Semantic (BadId "a b"))])
  , testCase "two forbid entries with the same literals" $
      problemsOf
        [ question
            "q"
            [ "title: Q"
            , "pick: any"
            , "options:"
            , "  a: {title: A}"
            , "  b: {title: B}"
            , "forbid:"
            , "  - all: [a, not b]"
            , "  - all: [not b, a]"
            ]
            ["Q.", "", "## a", "", "A.", "", "## b", "", "B."]
        ]
        >>= (@?= [("questions/q.md", Just 9, Semantic (DuplicateForbid (Loc (QuestionDoc "q") (Just 8))))])
  , testCase "an empty raised-by" $
      problemsOf [question "q" ["title: Q", "pick: one", "raised-by: []"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Semantic (EmptyList RaisedByField))])
  , testCase "a link label not in kebab case" $
      problemsOf [question "q" ["title: Q", "pick: one", "links: [{rel: Rivals, to: q}]"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Semantic (BadLabel "Rivals"))])
  , testCase "a link to nothing" $
      problemsOf [question "q" ["title: Q", "pick: one", "links: [{rel: gains, to: n}]"] ["Q."]]
        >>= (@?= [("questions/q.md", Just 4, Semantic (UnknownTarget "n"))])
  , testCase "a note without a body" $
      problemsOf [("notes/n.md", "---\ntitle: N\n---\n")]
        >>= (@?= [("notes/n.md", Nothing, Semantic MissingDescription)])
  ]

grades :: [TestTree]
grades =
  [ testCase "reads the grades and an option's assessment" $ do
      (ps, m) <-
        loadFiles
          [ config ["grades: [broken, hack, clean]"]
          , graded ["  a: {title: A, assessment: {grade: hack, unverified: true, why: slow}}"]
          ]
      ps @?= []
      [m'.space.grades | Just m' <- [m]] @?= [["broken", "hack", "clean"]]
      [o.assessment | Just m' <- [m], o <- (m'.space.questions Map.! Id "q").options]
        @?= [Just Assessment {grade = "hack", unverified = True, why = Just "slow"}]
  , testCase "a grade the space does not declare" $
      problemsOf [config ["grades: [low, high]"], graded ["  a: {title: A, assessment: {grade: mid}}"]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (UnknownGrade "mid"))])
  , testCase "an assessment in a space declaring no grades" $
      problemsOf [graded ["  a: {title: A, assessment: {grade: mid}}"]]
        >>= (@?= [("questions/q.md", Just 5, Semantic NoGrades)])
  , testCase "an assessment without a grade" $
      problemsOf [config ["grades: [low, high]"], graded ["  a: {title: A, assessment: {why: slow}}"]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (Missing GradeField))])
  , testCase "unverified is not true" $
      problemsOf [config ["grades: [low, high]"], graded ["  a: {title: A, assessment: {grade: low, unverified: yes}}"]]
        >>= (@?= [("questions/q.md", Just 5, Semantic (BadUnverified "yes"))])
  , testCase "a grade listed twice" $
      problemsOf [config ["grades: [low, high, low]"]]
        >>= (@?= [("quandary.yaml", Just 1, Semantic (DuplicateGrade "low"))])
  , testCase "a bad grade name" $
      problemsOf [config ["grades: [low, so so]"]]
        >>= (@?= [("quandary.yaml", Just 1, Semantic (BadGrade "so so"))])
  , testCase "empty grades" $
      problemsOf [config ["grades: []"]]
        >>= (@?= [("quandary.yaml", Just 1, Semantic (EmptyList GradesField))])
  , testCase "an unknown key in quandary.yaml" $
      problemsOf [config ["grades: [low]", "scale: 3"]]
        >>= (@?= [("quandary.yaml", Just 2, Format (UnknownKey "scale"))])
  ]
  where
    config ls = ("quandary.yaml", Text.unlines ls)
    graded opts = question "q" (["title: Q", "pick: one", "options:"] ++ opts) ["Q.", "", "## a", "", "A."]

pointProblems :: [TestTree]
pointProblems =
  [ testCase "a title gone out of date does not block" $ do
      (ps, m) <- loadFiles [space, point "current" ["accept:", "  a: {title: Old A}"]]
      ps @?= [("points/current.md", Just 3, Semantic (TitleMismatch (Listed (Id "a")) "Old A" "A"))]
      isJust m @?= True
  , testCase "a ruling without a title" $
      problemsOf [space, point "current" ["accept:", "  a: {}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic (Missing TitleField))])
  , testCase "a ruling on other with a title" $
      problemsOf [space, point "current" ["defeat:", "  q.other: {title: O, reason: r}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic (TitleOnOther (Other (Id "q"))))])
  , testCase "a defeat without a reason" $
      problemsOf [space, point "current" ["defeat:", "  a: {title: A}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic (Missing ReasonField))])
  , testCase "an accept with an empty reason" $
      problemsOf [space, point "current" ["accept:", "  a: {title: A, reason: \"\"}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic EmptyReason)])
  , testCase "a defeat with a blank reason" $
      problemsOf [space, point "current" ["defeat:", "  a: {title: A, reason: \"  \"}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic EmptyReason)])
  , testCase "accepted and defeated in one point" $
      problemsOf [space, point "current" ["accept:", "  a: {title: A}", "defeat:", "  a: {title: A, reason: r}"]]
        >>= (@?= [("points/current.md", Nothing, Semantic (AcceptedAndDefeated (Listed (Id "a"))))])
  , testCase "a point's own ruling may differ from an inherited one" $
      problemsOf
        [ space
        , point "current" ["accept:", "  a: {title: A}"]
        , point "branch" ["extends: current", "defeat:", "  a: {title: A, reason: r}"]
        ]
        >>= (@?= [])
  , testCase "dropping what no extended point rules" $
      problemsOf [space, point "current" [], point "branch" ["extends: current", "drop: [a]"]]
        >>= (@?= [("points/branch.md", Nothing, Semantic (DropsNothing (Listed (Id "a"))))])
  , testCase "dropping what the point rules itself" $
      problemsOf
        [ space
        , point "current" ["accept:", "  a: {title: A}"]
        , point "branch" ["extends: current", "accept:", "  a: {title: A}", "drop: [a]"]
        ]
        >>= (@?= [("points/branch.md", Nothing, Semantic (DropsOwn (Listed (Id "a"))))])
  , testCase "dropping what an intermediate point already dropped" $
      problemsOf
        [ space
        , point "current" ["accept:", "  a: {title: A}"]
        , point "middle" ["extends: current", "drop: [a]"]
        , point "branch" ["extends: middle", "drop: [a]"]
        ]
        >>= (@?= [("points/branch.md", Nothing, Semantic (DropsNothing (Listed (Id "a"))))])
  , testCase "a question where an option belongs" $
      problemsOf [space, point "current" ["accept:", "  q: {title: Q}"]]
        >>= (@?= [("points/current.md", Just 3, Semantic (QuestionNotOption "q"))])
  , testCase "extending an unknown point" $
      problemsOf [space, point "branch" ["extends: nowhere"]]
        >>= (@?= [("points/branch.md", Just 2, Semantic (UnknownPoint "nowhere"))])
  , testCase "an extends cycle" $
      problemsOf [space, point "x" ["extends: y"], point "y" ["extends: x"]]
        >>= ( @?=
                [ ("points/x.md", Nothing, Semantic (ExtendsCycle ["x", "y", "x"]))
                , ("points/y.md", Nothing, Semantic (ExtendsCycle ["y", "x", "y"]))
                ]
            )
  , testCase "a rank out of range" $
      problemsOf [space, point "current" ["prefer: {a: 11}"]]
        >>= (@?= [("points/current.md", Just 2, Semantic (BadRank "11"))])
  , testCase "deferring an unknown question" $
      problemsOf [space, point "current" ["defer: [nowhere]"]]
        >>= (@?= [("points/current.md", Just 2, Semantic (UnknownQuestion "nowhere"))])
  ]
  where
    space = question "q" ["title: Q", "pick: one", "options:", "  a: {title: A}"] ["Q.", "", "## a", "", "A."]
    point n front = ("points/" <> n <> ".md", Text.unlines (["---"] ++ front ++ ["---"]))

bodies :: [TestTree]
bodies =
  [ testCase "headings in code and setext headings" $ do
      (ps, m) <-
        loadFiles
          [ question
              "q"
              ["title: Q", "pick: one", "options:", "  a: {title: A}"]
              ["Q.", "", "```", "## a", "```", "", "a", "--", "", "A."]
          ]
      ps @?= []
      [o.description | Just m' <- [m], o <- (m'.space.questions Map.! Id "q").options] @?= ["A."]
  ]

type Seen = (FilePath, Maybe Int, FileIssue)

question :: FilePath -> [Text] -> [Text] -> (FilePath, Text)
question q front body = ("questions/" <> q <> ".md", Text.unlines (["---"] ++ front ++ ["---"] ++ body))

problemsOf :: [(FilePath, Text)] -> IO [Seen]
problemsOf files = fst <$> loadFiles files

loadFiles :: [(FilePath, Text)] -> IO ([Seen], Maybe Model)
loadFiles files = withSystemTempDirectory "quandary" $ \dir -> do
  mapM_
    ( \(path, content) -> do
        createDirectoryIfMissing True (takeDirectory (dir </> path))
        Text.writeFile (dir </> path) content
    )
    files
  (ps, m) <- load dir
  pure ([(makeRelative dir p.file, p.line, p.issue) | p <- ps], m)

load :: FilePath -> IO ([FileProblem], Maybe Model)
load dir = do
  loaded <- readSpace dir
  let v = validate loaded.sources
  pure (Files.problemsOf dir loaded v, v.model)
