module Quandary.Shell.Cli (main) where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Validate
import Quandary.Shell.Command.Accept qualified as Accept
import Quandary.Shell.Command.Adopt qualified as Adopt
import Quandary.Shell.Command.Browse qualified as Browse
import Quandary.Shell.Command.Check qualified as Check
import Quandary.Shell.Command.Defeat qualified as Defeat
import Quandary.Shell.Command.Defer qualified as Defer
import Quandary.Shell.Command.Diff qualified as Diff
import Quandary.Shell.Command.Exhaust qualified as Exhaust
import Quandary.Shell.Command.Graph qualified as Graph
import Quandary.Shell.Command.Init qualified as Init
import Quandary.Shell.Command.Links qualified as Links
import Quandary.Shell.Command.Point qualified as Point
import Quandary.Shell.Command.Prefer qualified as Prefer
import Quandary.Shell.Command.Reopen qualified as Reopen
import Quandary.Shell.Command.Rulings qualified as Rulings
import Quandary.Shell.Command.Serve qualified as Serve
import Quandary.Shell.Command.Settle qualified as Settle
import Quandary.Shell.Command.Show qualified as Show
import Quandary.Shell.Command.Status qualified as Status
import Quandary.Shell.Command.Whatif qualified as Whatif
import Quandary.Shell.Command.Why qualified as Why
import Quandary.Shell.Env (Env (..))
import Quandary.Shell.Exit (failWith, warn)
import Quandary.Shell.Options (Access (..), Action (..), Command (..), Options (..), SpaceCommand (..))
import Quandary.Shell.Options qualified as Options
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Storage.Files (LockState (..), Override (..))
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

main :: IO ()
main = do
  o <- Options.parse
  code <- run o `catch` unexpected
  exitWith code

-- | Report a failed read or write without a backtrace.
unexpected :: IOException -> IO ExitCode
unexpected e = failWith (Text.pack (show e))

run :: Options -> IO ExitCode
run o = case o.command of
  Init -> Init.run o.dir
  InSpace command -> do
    space <- Files.isSpace o.dir
    if space
      then inSpace o command
      else failWith ("not a space: " <> Text.pack o.dir <> " has no quandary.yaml (quandary init creates one)")

inSpace :: Options -> SpaceCommand -> IO ExitCode
inSpace o command = case command of
  Browse -> Browse.run o.dir o.point
  Serve address port -> Serve.run o.dir o.point address port
  Check -> check
  Lock reason -> lockFor o.dir reason
  Unlock -> do
    code <- check
    (state, forced) <- Files.unlockEdit o.dir
    code <$ Text.putStr (Text.unlines (Render.unlocked state forced))
  OnModel access action -> guarded o.dir access $ \override -> do
    (loaded, v) <- validated
    forM_ (Files.problemsOf o.dir loaded v) (Text.hPutStrLn stderr . Render.problem o.dir)
    case Files.usableModel loaded v of
      Just m -> dispatch (Env o.dir o.point m override) action
      Nothing -> failWith "the space has problems; nothing done"
  where
    check = do
      (loaded, v) <- validated
      Check.run o.dir (Files.problemsOf o.dir loaded v) v.model
    validated = do
      loaded <- Files.readSpace o.dir
      pure (loaded, validate loaded.sources)

-- | Run the command against the edit lock: warn of it when reading; when
-- writing, hold the ruling lock and refuse while the edit lock is live,
-- unless forced.
guarded :: FilePath -> Access -> (Override -> IO ExitCode) -> IO ExitCode
guarded dir Reading k = Files.editLock dir >>= warnOf >> k KeepLock
guarded dir Writing k =
  Files.withRulingLock dir $
    Files.editLock dir >>= \case
      Locked l -> failWith ("the space is " <> Render.locked l <> "; nothing done (--force writes anyway)")
      state -> warnOf state >> k KeepLock
guarded dir Forcing k =
  Files.withRulingLock dir $
    Files.editLock dir >>= \case
      Locked _ -> k OverrideLock
      state -> warnOf state >> k KeepLock

-- | Print the lock, live or lapsed, as a warning.
warnOf :: LockState -> IO ()
warnOf = mapM_ warn . Render.lockNotice

-- | Lock the space for an edit, for the reason.
lockFor :: FilePath -> Text -> IO ExitCode
lockFor dir reason
  | Text.null (Text.strip reason) = failWith "lock needs a reason"
  | otherwise =
      Files.lockForEdit dir reason >>= \case
        Left held -> failWith ("the space is already " <> Render.locked held)
        Right stored -> ExitSuccess <$ Text.putStrLn ("locked for an edit: " <> stored)

dispatch :: Env -> Action -> IO ExitCode
dispatch env Status = Status.run env
dispatch env (Show q) = Show.run env q
dispatch env (Why t) = Why.run env t
dispatch env (Whatif args) = Whatif.run env args
dispatch env (Accept args) = Accept.run env args
dispatch env (Defeat args) = Defeat.run env args
dispatch env (Exhaust args) = Exhaust.run env args
dispatch env (Settle args) = Settle.run env args
dispatch env (Reopen ts) = Reopen.run env ts
dispatch env (Defer q) = Defer.run env q
dispatch env (Prefer t n) = Prefer.run env t n
dispatch env (PointNew name parent proposed) = Point.create env name parent proposed
dispatch env (PointRm name) = Point.remove env name
dispatch env (Adopt name confirmation) = Adopt.run env name confirmation
dispatch env Rulings = Rulings.run env
dispatch env Links = Links.run env
dispatch env (Diff a b) = Diff.run env a b
dispatch env (Graph f) = Graph.run env f
