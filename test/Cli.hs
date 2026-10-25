module Cli (cli) where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (addUTCTime, getCurrentTime)
import Fixtures (ageSpace, withCopy)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, setModificationTime)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (readFile')
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

cli :: TestTree
cli =
  testGroup
    "cli"
    [ testCase "check on a clean space" $ withCopy $ \dir -> do
        r <- quandary dir ["check"]
        r @?= (ExitSuccess, "ok\n", "")
    , testCase "check reports a problem with its file and line" $ withCopy $ \dir -> do
        Text.writeFile (dir </> "questions/store/where.md") . Text.replace "pick: one" "pick: one\nrank: 3"
          =<< Text.readFile (dir </> "questions/store/where.md")
        (code, out, _) <- quandary dir ["check"]
        code @?= ExitFailure 1
        lines out @?= [dir </> "questions/store/where.md:4: unknown key rank"]
    , testCase "check reports a stale point" $ withCopy $ \dir -> do
        writeFile
          (dir </> "points/broken.md")
          "---\naccept:\n  sync/crdt: {title: CRDT per note}\n  store/files: {title: Plain Markdown files in a folder}\n---\n"
        (code, out, _) <- quandary dir ["check"]
        code @?= ExitFailure 1
        lines out @?= [dir </> "points/broken.md: the point is stale"]
    , testCase "every command but init refuses a directory without quandary.yaml" $ withCopy $ \dir -> do
        removeFile (dir </> "quandary.yaml")
        let missing = dir </> "missing"
        let commands = [["check"], ["status"], ["browse"], ["serve"]]
        results <- sequence [quandary d args | d <- [dir, missing], args <- commands]
        [(code, err) | (code, _, err) <- results]
          @?= [ (ExitFailure 1, "quandary: not a space: " <> d <> " has no quandary.yaml (quandary init creates one)\n")
              | d <- [dir, missing]
              , _ <- commands
              ]
    , testCase "init creates a space that checks clean" $ withCopy $ \dir -> do
        let new = dir </> "new"
        (code, out, _) <- quandary new ["init"]
        (code, lines out)
          @?= ( ExitSuccess
              , map
                  (("created " <>) . (new </>))
                  ["", "questions", "notes", "points", "quandary.yaml", ".quandary/.gitignore"]
              )
        ignore <- readFile' (new </> ".quandary/.gitignore")
        ignore @?= "*\n"
        r <- quandary new ["check"]
        r @?= (ExitSuccess, "ok\n", "")
    , testCase "init creates only what is missing" $ withCopy $ \dir -> do
        removeFile (dir </> "quandary.yaml")
        writeFile (dir </> "notes/kept.md") "kept"
        (code, out, _) <- quandary dir ["init"]
        (code, lines out)
          @?= (ExitSuccess, ["created " <> dir </> "quandary.yaml", "created " <> dir </> ".quandary/.gitignore"])
        kept <- readFile' (dir </> "notes/kept.md")
        kept @?= "kept"
    , testCase "init on a whole space overwrites nothing" $ withCopy $ \dir -> do
        _ <- quandary dir ["init"]
        writeFile (dir </> "quandary.yaml") "grades: [low, high]\n"
        r <- quandary dir ["init"]
        r @?= (ExitSuccess, "already a space\n", "")
        config <- readFile' (dir </> "quandary.yaml")
        config @?= "grades: [low, high]\n"
    , testCase "an empty or comment-only quandary.yaml configures nothing" $ withCopy $ \dir -> do
        results <-
          mapM (\config -> writeFile (dir </> "quandary.yaml") config >> quandary dir ["check"]) ["", "\n", "# grades: [a, b]\n"]
        results @?= replicate 3 (ExitSuccess, "ok\n", "")
    , testCase "a ruling creating .quandary/ ignores what it holds" $ withCopy $ \dir -> do
        _ <- quandary dir ["defeat", "conflict/copy", "--", "not now"]
        files <- listDirectory (dir </> ".quandary")
        ignore <- readFile' (dir </> ".quandary/.gitignore")
        (sort files, ignore) @?= ([".gitignore", "rulings.lock"], "*\n")
    , testCase "a command refuses a space with problems" $ withCopy $ \dir -> do
        writeFile (dir </> "questions/bad.md") "no frontmatter\n"
        (code, out, err) <- quandary dir ["status"]
        code @?= ExitFailure 1
        out @?= ""
        assertBool err ("quandary: the space has problems; nothing done" `isInfixOf` err)
    , testCase "accept writes the point and prints what moved" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["accept", "sync/git", "history", "for", "free"]
        code @?= ExitSuccess
        lines out @?= ["accepted sync/git", "  sync/how: open -> closed"]
        point <- readFile' (dir </> "points/current.md")
        assertBool point ("sync/git: {title: Git repo under the hood, reason: history for free}" `isInfixOf` point)
    , testCase "a ruling that leaves the point stale is refused and writes nothing" $ withCopy $ \dir -> do
        before <- readFile' (dir </> "points/current.md")
        (code, out, err) <- quandary dir ["accept", "sync/crdt"]
        code @?= ExitFailure 1
        out @?= ""
        lines err
          @?= [ "quandary: refused: no design would reach the point, because of:"
              , "  accept store/files (current: must open in any editor)"
              , "  accept sync/crdt"
              ]
        later <- readFile' (dir </> "points/current.md")
        later @?= before
    , testCase "whatif writes nothing" $ withCopy $ \dir -> do
        before <- readFile' (dir </> "points/current.md")
        (code, out, _) <- quandary dir ["whatif", "defeat", "sync/git"]
        code @?= ExitSuccess
        assertBool out ("sync/how: open -> closed (other forced; git impossible)" `isInfixOf` out)
        later <- readFile' (dir </> "points/current.md")
        later @?= before
    , testCase "an unknown option" $ withCopy $ \dir -> do
        r <- quandary dir ["why", "sync/nothing"]
        r @?= (ExitFailure 1, "", "quandary: unknown option sync/nothing\n")
    , testCase "a rank out of range" $ withCopy $ \dir -> do
        r <- quandary dir ["prefer", "sync/git", "11"]
        r @?= (ExitFailure 1, "", "quandary: rank 11: expected an integer from 0 to 10, or -\n")
    , testCase "a point that exists" $ withCopy $ \dir -> do
        r <- quandary dir ["point", "new", "current"]
        r @?= (ExitFailure 1, "", "quandary: point current exists\n")
    , testCase "a new point extending current" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["point", "new", "idea", "current"]
        (code, out) @?= (ExitSuccess, "created point idea extending current\n")
        point <- readFile' (dir </> "points/idea.md")
        point @?= "---\nextends: current\n---\n"
    , testCase "rulings at another point" $ withCopy $ \dir -> do
        (code, _, _) <- quandary dir ["--point", "edit-offline", "accept", "conflict/inbox"]
        code @?= ExitSuccess
        point <- readFile' (dir </> "points/edit-offline.md")
        assertBool point ("conflict/inbox:" `isInfixOf` point)
        assertBool point ("Edit the same note on phone and laptop offline." `isInfixOf` point)
    , testCase "a point's own ruling replaces an inherited one" $ withCopy $ \dir -> do
        (code, _, err) <- quandary dir ["--point", "edit-offline", "defeat", "store/files", "phones", "first"]
        code @?= ExitFailure 1
        lines err
          @?= [ "quandary: refused: no design would reach the point, because of:"
              , "  accept sync/git (edit-offline)"
              , "  defeat store/files"
              , "and because of:"
              , "  accept conflict/diff (edit-offline)"
              , "  defeat store/files"
              , "  defeat sync/lww (current: silently loses edits made offline)"
              , "reopen drops an inherited ruling at this point"
              ]
        (code', _, _) <- quandary dir ["--point", "edit-offline", "defeat", "conflict/diff", "too", "busy"]
        code' @?= ExitSuccess
        point <- readFile' (dir </> "points/edit-offline.md")
        assertBool point ("conflict/diff: {title: Side-by-side merge screen, reason: too busy}" `isInfixOf` point)
        assertBool point (not ("accept:\n  conflict/diff" `isInfixOf` point))
    , testCase "reopening an inherited ruling drops it and says what still forces the option" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["--point", "edit-offline", "reopen", "store/files"]
        code @?= ExitSuccess
        lines out
          @?= [ "reopened store/files"
              , "  nothing moved"
              , "  store/files is still forced because of:"
              , "    accept sync/git (edit-offline)"
              , "  and because of:"
              , "    accept conflict/diff (edit-offline)"
              , "    defeat sync/lww (current: silently loses edits made offline)"
              ]
        point <- readFile' (dir </> "points/edit-offline.md")
        assertBool point ("drop: [store/files]" `isInfixOf` point)
    , testCase "rulings lists inherited ones with their points" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["--point", "edit-offline", "rulings"]
        code @?= ExitSuccess
        lines out
          @?= [ "point edit-offline extends current"
              , ""
              , "accept conflict/diff (edit-offline)"
              , "accept store/files (current: must open in any editor)"
              , "accept sync/git (edit-offline)"
              , "defeat conflict/silent (current: data loss)"
              , "defeat sync/lww (current: silently loses edits made offline)"
              ]
    , testCase "diff shows the questions and options that differ" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["diff", "current", "edit-offline"]
        code @?= ExitSuccess
        assertBool out ("sync/how         open      closed" `isInfixOf` out)
        assertBool out ("  sync/git       possible  accepted" `isInfixOf` out)
        assertBool out (not ("other" `isInfixOf` out))
    , testCase "whatif takes several verbs and reports points left stale" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["whatif", "reopen", "store/files", "defeat", "store/mirror", "sync/lww"]
        code @?= ExitSuccess
        assertBool out ("store/where: closed -> open (files, sqlite possible)" `isInfixOf` out)
        (code', out', _) <- quandary dir ["whatif", "defeat", "store/files"]
        code' @?= ExitSuccess
        assertBool out' ("point edit-offline is now stale" `isInfixOf` out')
    , testCase "whatif names the rulings a stale point cannot hold together" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["whatif", "accept", "sync/crdt"]
        code @?= ExitSuccess
        take 3 (lines out)
          @?= [ "the point is now stale, because of:"
              , "  accept store/files (current: must open in any editor)"
              , "  accept sync/crdt (current)"
              ]
    , testCase "whatif takes no reasons" $ withCopy $ \dir -> do
        r <- quandary dir ["whatif", "defeat", "sync/git", "because"]
        r @?= (ExitFailure 1, "", "quandary: unknown id because: whatif takes verbs and ids, no reasons\n")
    , testCase "a question where an option belongs" $ withCopy $ \dir -> do
        r <- quandary dir ["accept", "sync/how"]
        r @?= (ExitFailure 1, "", "quandary: sync/how is a question, not an option\n")
    , testCase "exhaust rules out unlisted answers" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["exhaust", "sync/how", "all", "there", "is"]
        code @?= ExitSuccess
        lines out @?= ["exhausted sync/how", "  sync/how: open -> closed (git forced)"]
    , testCase "exhaust takes several questions and one reason" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["exhaust", "sync/how", "conflict/shown", "--", "all", "there", "is"]
        code @?= ExitSuccess
        filter (not . isPrefixOf " ") (lines out) @?= ["exhausted sync/how", "exhausted conflict/shown"]
    , testCase "exhaust names an option where a question belongs" $ withCopy $ \dir -> do
        r <- quandary dir ["exhaust", "sync/how", "sync/git", "--", "all"]
        r @?= (ExitFailure 1, "", "quandary: sync/git is an option, not a question\n")
    , testCase "links lists each label in use" $ withCopy $ \dir -> do
        r <- quandary dir ["links"]
        r @?= (ExitSuccess, "link labels\n  gains  1\n", "")
    , testCase "accepting under pick any prints only what else moved" $ withCopy $ \dir -> do
        writeFixes dir
        r <- quandary dir ["accept", "extra/a", "--", "cheap"]
        r @?= (ExitSuccess, "accepted extra/a\n", "")
    , testCase "settle defeats what is still possible" $ withCopy $ \dir -> do
        writeFixes dir
        _ <- quandary dir ["accept", "extra/a", "--", "cheap"]
        (code, out, _) <- quandary dir ["settle", "extra/fix", "--", "all", "of", "it"]
        code @?= ExitSuccess
        lines out @?= ["settled extra/fix (b, live, other defeated)", "  extra/fix: open -> closed"]
    , testCase "show gives grades, bounds and dominance" $ withCopy $ \dir -> do
        writeGraded dir
        r <- quandary dir ["show", "extra/pick"]
        r
          @?= ( ExitSuccess
              , unlines
                  [ "extra/pick: Pick [open]"
                  , "pick: one, a root"
                  , ""
                  , "Pick."
                  , ""
                  , "possible    extra/a           A  value clean"
                  , "  graded clean: simple"
                  , "possible    extra/b           B  value hack"
                  , "  graded hack"
                  , "  dominated: at best hack, below extra/a at worst clean"
                  , "possible    extra/c           C  value broken..costly"
                  , "  graded costly, unverified"
                  , "  dominated: at best costly, below extra/a at worst clean"
                  , "possible    extra/pick.other  value broken..clean"
                  ]
              , ""
              )
    , testCase "show on a possible option gives its question, body and the questions it raises" $ withCopy $ \dir -> do
        r <- quandary dir ["show", "sync/git"]
        r
          @?= ( ExitSuccess
              , unlines
                  [ "sync/git: Git repo under the hood [possible]"
                  , "in sync/how: How devices sync [open]"
                  , ""
                  , "The notes folder is a git repository; sync is commit, pull and push."
                  , ""
                  , "raises conflict/shown: What the user sees on a conflict"
                  , "raises sync/merge-conflicts: Merge conflicts surface to non-technical users"
                  ]
              , ""
              )
    , testCase "show on an option defeated for no other cause gives its ruling alone" $ withCopy $ \dir -> do
        r <- quandary dir ["show", "sync/lww"]
        r
          @?= ( ExitSuccess
              , unlines
                  [ "sync/lww: Last write wins per file [defeated]"
                  , "in sync/how: How devices sync [open]"
                  , "defeated: silently loses edits made offline"
                  , ""
                  , "The most recently written copy of a file replaces the others."
                  , ""
                  , "raises conflict/shown: What the user sees on a conflict"
                  ]
              , ""
              )
    , testCase "show on a defeated option impossible for another cause gives every set of rulings" $ withCopy $ \dir -> do
        _ <- quandary dir ["defeat", "sync/crdt", "--", "too", "complex"]
        (_, out, _) <- quandary dir ["show", "sync/crdt"]
        take 10 (lines out)
          @?= [ "sync/crdt: CRDT per note [defeated]"
              , "in sync/how: How devices sync [open]"
              , "defeated: too complex"
              , "impossible because of:"
              , "  accept store/files (current: must open in any editor)"
              , "  through:"
              , "    sync/crdt needs store/sqlite"
              , "    store/where takes at most one option"
              , "and because of:"
              , "  defeat sync/crdt (current: too complex)"
              ]
    , testCase "show on an impossible option gives every set of rulings responsible" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["show", "sync/crdt"]
        code @?= ExitSuccess
        take 4 (lines out)
          @?= [ "sync/crdt: CRDT per note [impossible]"
              , "in sync/how: How devices sync [open]"
              , "impossible because of:"
              , "  accept store/files (current: must open in any editor)"
              ]
    , testCase "show on a forced option gives every set of rulings responsible" $ withCopy $ \dir -> do
        _ <- quandary dir ["exhaust", "sync/how", "--", "all", "there", "is"]
        (code, out, _) <- quandary dir ["show", "sync/git"]
        code @?= ExitSuccess
        take 6 (lines out)
          @?= [ "sync/git: Git repo under the hood [forced]"
              , "in sync/how: How devices sync [closed]"
              , "forced because of:"
              , "  accept store/files (current: must open in any editor)"
              , "  defeat sync/lww (current: silently loses edits made offline)"
              , "  defeat sync/how.other (current: all there is)"
              ]
    , testCase "show on an option gives the point an inherited ruling comes from" $ withCopy $ \dir -> do
        (_, out, _) <- quandary dir ["--point", "edit-offline", "show", "store/files"]
        take 5 (lines out)
          @?= [ "store/files: Plain Markdown files in a folder [accepted]"
              , "in store/where: Where notes live on disk [closed]"
              , "accepted in current: must open in any editor"
              , "forced because of:"
              , "  accept store/files (current: must open in any editor)"
              ]
    , testCase "show on an implicit other" $ withCopy $ \dir -> do
        r <- quandary dir ["show", "sync/how.other"]
        r @?= (ExitSuccess, unlines ["sync/how.other [possible]", "in sync/how: How devices sync [open]"], "")
    , testCase "show on an option gives grades, bounds and dominance" $ withCopy $ \dir -> do
        writeGraded dir
        (_, out, _) <- quandary dir ["show", "extra/b"]
        take 5 (lines out)
          @?= [ "extra/b: B [possible]"
              , "in extra/pick: Pick [open]"
              , "value hack"
              , "graded hack"
              , "dominated: at best hack, below extra/a at worst clean"
              ]
    , testCase "show on an unknown id" $ withCopy $ \dir -> do
        r <- quandary dir ["show", "sync/nope"]
        r @?= (ExitFailure 1, "", "quandary: unknown question or option sync/nope\n")
    , testCase "status gives bounds that say something" $ withCopy $ \dir -> do
        writeGraded dir
        (code, out, _) <- quandary dir ["status"]
        code @?= ExitSuccess
        filter ("extra/pick" `isInfixOf`) (lines out) @?= ["  extra/pick  a clean, b hack, c broken..costly"]
    , testCase "an option's bounds take in the questions it raises" $ withCopy $ \dir -> do
        writeGraded dir
        let path = dir </> "questions/sync/merge-conflicts.md"
        before <- Text.readFile path
        Text.writeFile
          path
          ( Text.replace
              "    title: An automatic merge driver for Markdown\n"
              "    title: An automatic merge driver for Markdown\n    assessment: {grade: hack}\n"
              before
          )
        _ <- quandary dir ["exhaust", "sync/merge-conflicts", "--", "the driver or nothing"]
        (_, out, _) <- quandary dir ["show", "sync/how"]
        filter ("sync/git " `isInfixOf`) (lines out)
          @?= ["possible    sync/git        Git repo under the hood  value broken..hack"]
    , testCase "show prints each question an option raises once, with its co-raisers" $ withHistory $ \dir -> do
        (_, out, _) <- quandary dir ["show", "sync/how"]
        drop 1 (dropWhile (not . ("defeated" `isPrefixOf`)) (lines out))
          @?= [ "  raises conflict/shown: What the user sees on a conflict"
              , "possible    sync/git        Git repo under the hood  value broken..clean"
              , "  raises conflict/shown: What the user sees on a conflict"
              , "  raises extra/history: What history keeps"
              , "    with any of extra/a, extra/b"
              , "  raises sync/merge-conflicts: Merge conflicts surface to non-technical users"
              , "possible    sync/how.other  value broken..clean"
              ]
        (_, out', _) <- quandary dir ["show", "extra/pick"]
        take 3 (dropWhile (/= "  graded clean: simple") (lines out'))
          @?= ["  graded clean: simple", "  raises extra/history: What history keeps", "    with sync/git"]
    , testCase "show wraps co-raisers at 80 columns" $ withHistory $ \dir -> do
        editHistory
          dir
          "[sync/git, extra/b]]"
          "[sync/git, extra/b], [sync/git, extra/c], [sync/git, extra/pick.other], [sync/git, store/sqlite]]"
        (_, out, _) <- quandary dir ["show", "sync/how"]
        filter ("    " `isPrefixOf`) (lines out)
          @?= [ "    with any of extra/a, extra/b, extra/c, extra/pick.other, store/sqlite"
              ]
        editHistory dir "[sync/git, store/sqlite]]" "[sync/git, store/sqlite], [sync/git, conflict/three-way]]"
        (_, out', _) <- quandary dir ["show", "sync/how"]
        filter ("    " `isPrefixOf`) (lines out')
          @?= [ "    with any of extra/a, extra/b, extra/c, extra/pick.other, store/sqlite,"
              , "      conflict/three-way"
              ]
    , testCase "co-raisers needed together go in parentheses" $ withHistory $ \dir -> do
        editHistory dir "[sync/git, extra/b]]" "[sync/git, extra/b, store/files]]"
        (_, out, _) <- quandary dir ["show", "sync/how"]
        assertBool out ("    with any of extra/a, (extra/b with store/files)" `elem` lines out)
        (_, out', _) <- quandary dir ["show", "extra/pick"]
        assertBool
          out'
          (["  raises extra/history: What history keeps", "    with (sync/git with store/files)"] `isInfixOf` lines out')
        (_, out'', _) <- quandary dir ["whatif", "accept", "sync/git"]
        assertBool out'' ("    with any of extra/a, (extra/b with store/files)" `elem` lines out'')
    , testCase "whatif accept lists what the accepted options raise, with bounds at the new point" $ withHistory $ \dir -> do
        r <- quandary dir ["whatif", "accept", "extra/b"]
        r
          @?= ( ExitSuccess
              , unlines
                  [ "extra/pick: open -> closed (b forced; a, c impossible)"
                  , ""
                  , "raises"
                  , "  extra/history: What history keeps"
                  , "    with sync/git"
                  , "    all broken..costly, squash broken..hack"
                  ]
              , ""
              )
    , testCase "whatif accept lists raised questions live at the new point or not" $ withHistory $ \dir -> do
        (_, out, _) <- quandary dir ["whatif", "accept", "sync/git", "conflict/diff", "defeat", "extra/b"]
        dropWhile (/= "raises") (lines out)
          @?= [ "raises"
              , "  conflict/compare: What the merge screen compares"
              , "    two-way, three-way"
              , "  conflict/narrow: A phone screen is too narrow for two columns"
              , "    phone-layout, other"
              , "  conflict/shown: What the user sees on a conflict"
              , "    taken: diff"
              , "  conflict/when: When the user resolves"
              , "    modal, inbox"
              , "  extra/history: What history keeps"
              , "    with any of extra/a, extra/b"
              , "    all costly, squash hack"
              , "  sync/merge-conflicts: Merge conflicts surface to non-technical users"
              , "    driver, other"
              ]
        (_, out', _) <- quandary dir ["whatif", "accept", "sync/git", "defeat", "extra/a", "extra/b"]
        dropWhile (/= "  extra/history: What history keeps [dormant]") (lines out')
          @?= [ "  extra/history: What history keeps [dormant]"
              , "    with any of extra/a, extra/b"
              , "  sync/merge-conflicts: Merge conflicts surface to non-technical users"
              , "    driver, other"
              ]
    , testCase "whatif accept into a stale point lists nothing raised" $ withCopy $ \dir -> do
        (_, out, _) <- quandary dir ["whatif", "accept", "sync/crdt"]
        assertBool out ("raises" `notElem` lines out)
    , testCase "ranking an impossible option says so" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["prefer", "sync/crdt", "3"]
        code @?= ExitSuccess
        lines out @?= ["ranked sync/crdt 3", "  nothing moved", "  sync/crdt is impossible at this point"]
    , testCase "status at a stale point fails" $ withCopy $ \dir -> do
        writeFile
          (dir </> "points/broken.md")
          "---\naccept:\n  sync/crdt: {title: CRDT per note}\n  store/files: {title: Plain Markdown files in a folder}\n---\n"
        (code, out, _) <- quandary dir ["--point", "broken", "status"]
        code @?= ExitFailure 1
        assertBool out ("stale: no design holds these rulings:" `isInfixOf` out)
    , testCase "settle at a stale point is refused" $ withCopy $ \dir -> do
        writeFile
          (dir </> "points/broken.md")
          "---\naccept:\n  sync/crdt: {title: CRDT per note}\n  store/files: {title: Plain Markdown files in a folder}\n---\n"
        before <- readFile' (dir </> "points/broken.md")
        r <- quandary dir ["--point", "broken", "settle", "sync/how", "--", "all", "of", "it"]
        r @?= (ExitFailure 1, "", "quandary: sync/how cannot be settled: the point is stale\n")
        later <- readFile' (dir </> "points/broken.md")
        later @?= before
    , testCase "reopen takes several ids" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["--point", "edit-offline", "reopen", "sync/git", "conflict/diff", "store/files"]
        code @?= ExitSuccess
        filter (not . isPrefixOf " ") (lines out) @?= ["reopened sync/git", "reopened conflict/diff", "reopened store/files"]
        point <- readFile' (dir </> "points/edit-offline.md")
        assertBool point ("drop: [store/files]" `isInfixOf` point)
    , testCase "ruling again changes the reason" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["defeat", "sync/lww", "loses", "edits"]
        (code, lines out)
          @?= (ExitSuccess, ["defeated sync/lww", "  nothing moved", "  reason: silently loses edits made offline -> loses edits"])
    , testCase "a pick-one question named in a refusal" $ withCopy $ \dir -> do
        _ <- quandary dir ["exhaust", "store/where", "all", "there", "is"]
        (_, _, err) <- quandary dir ["accept", "store/sqlite"]
        assertBool err ("store/where is pick: one" `isInfixOf` err)
    , testCase "adopt moves a branch into current and keeps siblings fresh" $ withCopy $ \dir -> do
        _ <- quandary dir ["point", "new", "db", "current"]
        _ <- quandary dir ["--point", "db", "reopen", "store/files"]
        _ <- quandary dir ["--point", "db", "accept", "store/sqlite"]
        _ <- quandary dir ["--point", "db", "accept", "sync/crdt"]
        (code, out, _) <- quandary dir ["adopt", "db"]
        code @?= ExitSuccess
        let ls = lines out
        take 1 ls @?= ["adopted db into current"]
        assertBool out ("  point edit-offline drops store/sqlite, sync/crdt" `elem` ls)
        drop (length ls - 1) ls @?= ["removed point db"]
        point <- readFile' (dir </> "points/current.md")
        assertBool point ("sync/crdt:" `isInfixOf` point && not ("store/files:" `isInfixOf` point))
        r <- quandary dir ["check"]
        r @?= (ExitSuccess, "ok\n", "")
    , testCase "adopt prunes what a sibling now inherits unchanged" $ withCopy $ \dir -> do
        _ <- quandary dir ["point", "new", "g", "current"]
        _ <- quandary dir ["--point", "g", "accept", "sync/git"]
        (code, _, _) <- quandary dir ["adopt", "g"]
        code @?= ExitSuccess
        point <- readFile' (dir </> "points/edit-offline.md")
        assertBool point (not ("sync/git" `isInfixOf` point) && "conflict/diff" `isInfixOf` point)
    , testCase "a proposed point is adopted only once confirmed" $ withCopy $ \dir -> do
        (_, created, _) <- quandary dir ["point", "new", "idea", "current", "--proposed"]
        created @?= "created proposed point idea extending current\n"
        _ <- quandary dir ["--point", "idea", "accept", "sync/git"]
        r <- quandary dir ["adopt", "idea"]
        r @?= (ExitFailure 1, "", "quandary: idea is proposed: confirm its rulings with the user, then adopt --confirmed\n")
        (code, _, _) <- quandary dir ["adopt", "idea", "--confirmed"]
        code @?= ExitSuccess
    , testCase "point rm removes a point nothing extends" $ withCopy $ \dir -> do
        r <- quandary dir ["point", "rm", "current"]
        r @?= (ExitFailure 1, "", "quandary: current cannot be removed\n")
        r' <- quandary dir ["point", "rm", "edit-offline"]
        r' @?= (ExitSuccess, "removed point edit-offline\n", "")
    , testCase "current exists before its file does" $ withCopy $ \dir -> do
        removeFile (dir </> "points/current.md")
        r <- quandary dir ["point", "new", "b", "current"]
        r @?= (ExitSuccess, "created point b extending current\n", "")
        r' <- quandary dir ["check"]
        r' @?= (ExitSuccess, "ok\n", "")
    , testCase "an empty reason" $ withCopy $ \dir -> do
        (code, _, _) <- quandary dir ["accept", "sync/git", ""]
        code @?= ExitSuccess
        point <- readFile' (dir </> "points/current.md")
        assertBool point ("sync/git: {title: Git repo under the hood}" `isInfixOf` point)
        r <- quandary dir ["defeat", "sync/crdt", " "]
        r @?= (ExitFailure 1, "", "quandary: defeat: the reason is empty\n")
    , testCase "accepting an answer names the raiser it commits to" $ withCopy $ \dir -> do
        (_, out, _) <- quandary dir ["accept", "conflict/diff"]
        assertBool out ("  commits to raiser sync/git, which nothing else rules" `elem` lines out)
    , testCase "accept takes several options and one reason" $ withCopy $ \dir -> do
        (code, out, _) <- quandary dir ["accept", "sync/git", "conflict/diff", "--", "one", "call"]
        code @?= ExitSuccess
        filter (not . isPrefixOf " ") (lines out) @?= ["accepted sync/git", "accepted conflict/diff"]
        point <- readFile' (dir </> "points/current.md")
        assertBool point ("conflict/diff: {title: Side-by-side merge screen, reason: one call}" `isInfixOf` point)
    , testCase "a batch with a refused ruling writes nothing" $ withCopy $ \dir -> do
        before <- readFile' (dir </> "points/current.md")
        (code, _, err) <- quandary dir ["accept", "sync/git", "sync/crdt"]
        code @?= ExitFailure 1
        assertBool err ("nothing ruled" `isInfixOf` err)
        later <- readFile' (dir </> "points/current.md")
        later @?= before
    , testCase "a reason starting with an unknown id" $ withCopy $ \dir -> do
        r <- quandary dir ["accept", "sync/git", "conflict/dif", "typo"]
        r @?= (ExitFailure 1, "", "quandary: unknown option conflict/dif\n")
    , testCase "an id with capitals after the options is an unknown option" $ withCopy $ \dir -> do
        r <- quandary dir ["defeat", "sync/lww", "Sync/Crdt", "because"]
        r @?= (ExitFailure 1, "", "quandary: unknown option Sync/Crdt\n")
    , testCase "an id with a dot after the options is an unknown option" $ withCopy $ \dir -> do
        r <- quandary dir ["defeat", "sync/lww", "sync/how.othr", "because"]
        r @?= (ExitFailure 1, "", "quandary: unknown option sync/how.othr\n")
    , testCase "whatif defeat names its supposed ruling without a reason" $ withCopy $ \dir -> do
        (_, out, _) <- quandary dir ["whatif", "defeat", "store/files"]
        assertBool out ("  defeat store/files (current)" `elem` lines out)
    , testCase "rulings run at once each land in the point" $ withCopy $ \dir -> do
        let defeated = ["conflict/copy", "conflict/silent", "conflict/two-way", "conflict/stacked", "conflict/swipe", "conflict/modal"]
        results <- forConcurrently defeated $ \o -> quandary dir ["defeat", o, "--", "not now"]
        [code | (code, _, _) <- results] @?= map (const ExitSuccess) defeated
        written <- readFile' (dir </> "points/current.md")
        sequence_ [assertBool (o <> " in\n" <> written) (("  " <> o <> ":") `isInfixOf` written) | o <- defeated]
    , testCase "a ruling leaves no temporary file beside the point" $ withCopy $ \dir -> do
        _ <- quandary dir ["defeat", "conflict/copy", "--", "not now"]
        files <- listDirectory (dir </> "points")
        sort files @?= ["current.md", "edit-offline.md"]
    , testCase "while the space is locked for an edit, every command writing points refuses" $ withCopy $ \dir -> do
        _ <- quandary dir ["point", "new", "branch"]
        (code, out, _) <- quandary dir ["lock", "agent", "adding", "options"]
        (code, out) @?= (ExitSuccess, "locked for an edit: agent adding options\n")
        before <- readFile' (dir </> "points/current.md")
        let writing =
              [ ["accept", "sync/git"]
              , ["defeat", "conflict/copy", "--", "not now"]
              , ["exhaust", "sync/how", "--", "that is all"]
              , ["settle", "sync/how", "--", "done"]
              , ["reopen", "sync/git"]
              , ["defer", "sync/how"]
              , ["prefer", "sync/git", "3"]
              , ["point", "new", "other"]
              , ["point", "rm", "branch"]
              , ["adopt", "branch"]
              ]
        results <- mapM (quandary dir) writing
        [code' | (code', _, _) <- results] @?= map (const (ExitFailure 1)) writing
        forM_ results $ \(_, _, err) -> assertBool err (refusedForLock err)
        unchanged <- readFile' (dir </> "points/current.md")
        unchanged @?= before
    , testCase "--force writes while the space is locked for an edit" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        (code, out, _) <- quandary dir ["accept", "sync/git", "--force"]
        (code, lines out) @?= (ExitSuccess, ["accepted sync/git", "  sync/how: open -> closed"])
    , testCase "read-only commands warn while the space is locked for an edit" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        (code, _, err) <- quandary dir ["status"]
        code @?= ExitSuccess
        assertBool err ("quandary: warning: the space is locked for an edit since " `isPrefixOf` err)
        assertBool err ("agent adding options\n" `isSuffixOf` err)
    , testCase "a second lock refuses while the first is live" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "first"]
        (code, _, err) <- quandary dir ["lock", "second"]
        code @?= ExitFailure 1
        assertBool err ("quandary: the space is already locked for an edit since " `isPrefixOf` err)
        assertBool err ("ago): first\n" `isSuffixOf` err)
    , testCase "unlock checks the space, then releases the lock" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        writeFile (dir </> "questions/bad.md") "no frontmatter\n"
        (code, out, _) <- quandary dir ["unlock"]
        code @?= ExitFailure 1
        last (lines out) @?= "unlocked: agent adding options"
        removeFile (dir </> "questions/bad.md")
        r <- quandary dir ["defeat", "conflict/copy", "--", "not now"]
        r @?= (ExitSuccess, "defeated conflict/copy\n", "")
    , testCase "unlock with no lock says so" $ withCopy $ \dir -> do
        r <- quandary dir ["unlock"]
        r @?= (ExitSuccess, "ok\nnot locked\n", "")
    , testCase "the lock lapses 15 minutes after the later of its taking and the last edit" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        ageSpace dir 16
        (code, out, err) <- quandary dir ["defeat", "conflict/copy", "--", "not now"]
        (code, out) @?= (ExitSuccess, "defeated conflict/copy\n")
        assertBool err ("quandary: warning: the edit lock lapsed at " `isPrefixOf` err)
        assertBool err (" without unlock: agent adding options; the edit may be unfinished\n" `isSuffixOf` err)
        (code', _, _) <- quandary dir ["lock", "again"]
        code' @?= ExitSuccess
    , testCase "a lapsed lock warns until unlock" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        ageSpace dir 16
        (_, _, warned) <- quandary dir ["status"]
        assertBool warned ("lapsed at " `isInfixOf` warned)
        (code, out, _) <- quandary dir ["unlock"]
        code @?= ExitSuccess
        assertBool out ("ok\nunlocked: agent adding options (lapsed at " `isPrefixOf` out)
        (_, _, err) <- quandary dir ["status"]
        err @?= ""
    , testCase "unlock lists the rulings forced over the lock" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        _ <- quandary dir ["accept", "sync/git", "--force"]
        _ <- quandary dir ["--point", "edit-offline", "defeat", "conflict/copy", "--force", "--", "not", "now"]
        r <- quandary dir ["unlock"]
        r
          @?= ( ExitSuccess
              , unlines
                  [ "ok"
                  , "unlocked: agent adding options"
                  , "ruled while locked: accepted sync/git at current"
                  , "ruled while locked: defeated conflict/copy at edit-offline"
                  ]
              , ""
              )
    , testCase "--force with no lock and a refused --force list nothing" $ withCopy $ \dir -> do
        _ <- quandary dir ["accept", "sync/git", "--force"]
        _ <- quandary dir ["lock", "agent adding options"]
        _ <- quandary dir ["accept", "sync/crdt", "--force"]
        r <- quandary dir ["unlock"]
        r @?= (ExitSuccess, "ok\nunlocked: agent adding options\n", "")
    , testCase "a new lock forgets the lapsed one's forced rulings" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "first"]
        _ <- quandary dir ["accept", "sync/git", "--force"]
        ageSpace dir 16
        _ <- quandary dir ["lock", "second"]
        r <- quandary dir ["unlock"]
        r @?= (ExitSuccess, "ok\nunlocked: second\n", "")
    , testCase "an edit keeps the lock live" $ withCopy $ \dir -> do
        _ <- quandary dir ["lock", "agent adding options"]
        ageSpace dir 16
        now <- getCurrentTime
        setModificationTime (dir </> "questions/sync/how.md") (addUTCTime (-5 * 60) now)
        (code, _, _) <- quandary dir ["defeat", "conflict/copy", "--", "not now"]
        code @?= ExitFailure 1
    , testCase "a usage error" $ withCopy $ \dir -> do
        (code, _, err) <- quandary dir ["frobnicate"]
        code @?= ExitFailure 1
        assertBool err ("Invalid argument" `isInfixOf` err || "Usage:" `isPrefixOf` err)
    , testCase "--version" $ do
        (code, out, _) <- readProcessWithExitCode "quandary" ["--version"] ""
        (code, out) @?= (ExitSuccess, "0.1.0\n")
    ]

refusedForLock :: String -> Bool
refusedForLock err =
  "quandary: the space is locked for an edit since " `isPrefixOf` err
    && "ago): agent adding options; nothing done (--force writes anyway)\n" `isSuffixOf` err

quandary :: FilePath -> [String] -> IO (ExitCode, String, String)
quandary dir args = readProcessWithExitCode "quandary" (["-C", dir] ++ args) ""

writeFixes :: FilePath -> IO ()
writeFixes dir = do
  createDirectoryIfMissing True (dir </> "questions/extra")
  writeFile (dir </> "questions/extra/fix.md") $
    unlines
      [ "---"
      , "title: Fix"
      , "pick: any"
      , "options:"
      , "  a: {title: A}"
      , "  b: {title: B}"
      , "  live: {title: Live with it}"
      , "---"
      , "Fix."
      , ""
      , "## extra/a"
      , ""
      , "A."
      , ""
      , "## extra/b"
      , ""
      , "B."
      , ""
      , "## extra/live"
      , ""
      , "Live."
      ]

writeGraded :: FilePath -> IO ()
writeGraded dir = do
  writeFile (dir </> "quandary.yaml") "grades: [broken, hack, costly, clean]\n"
  createDirectoryIfMissing True (dir </> "questions/extra")
  writeFile (dir </> "questions/extra/pick.md") $
    unlines
      [ "---"
      , "title: Pick"
      , "pick: one"
      , "options:"
      , "  a: {title: A, assessment: {grade: clean, why: simple}}"
      , "  b: {title: B, assessment: {grade: hack}}"
      , "  c: {title: C, assessment: {grade: costly, unverified: true}}"
      , "---"
      , "Pick."
      , ""
      , "## extra/a"
      , ""
      , "A."
      , ""
      , "## extra/b"
      , ""
      , "B."
      , ""
      , "## extra/c"
      , ""
      , "C."
      ]

withHistory :: (FilePath -> IO a) -> IO a
withHistory k = withCopy $ \dir -> do
  writeGraded dir
  writeHistory dir
  k dir

writeHistory :: FilePath -> IO ()
writeHistory dir =
  writeFile (dir </> "questions/extra/history.md") $
    unlines
      [ "---"
      , "title: What history keeps"
      , "pick: one"
      , "raised-by: [[sync/git, extra/a], [sync/git, extra/b]]"
      , "options:"
      , "  all: {title: Every commit, assessment: {grade: costly}}"
      , "  squash: {title: Squashed daily, assessment: {grade: hack}}"
      , "---"
      , "History."
      , ""
      , "## extra/all"
      , ""
      , "All."
      , ""
      , "## extra/squash"
      , ""
      , "Squash."
      ]

editHistory :: FilePath -> Text -> Text -> IO ()
editHistory dir old new = Text.writeFile path . Text.replace old new =<< Text.readFile path
  where
    path = dir </> "questions/extra/history.md"
