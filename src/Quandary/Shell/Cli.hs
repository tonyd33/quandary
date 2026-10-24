module Quandary.Shell.Cli (main) where

import Control.Monad (forM_)
import Data.Text.IO qualified as Text
import Quandary.Core.Validate
import Quandary.Shell.Command.Accept qualified as Accept
import Quandary.Shell.Command.Adopt qualified as Adopt
import Quandary.Shell.Command.Check qualified as Check
import Quandary.Shell.Command.Defeat qualified as Defeat
import Quandary.Shell.Command.Defer qualified as Defer
import Quandary.Shell.Command.Diff qualified as Diff
import Quandary.Shell.Command.Exhaust qualified as Exhaust
import Quandary.Shell.Command.Graph qualified as Graph
import Quandary.Shell.Command.Links qualified as Links
import Quandary.Shell.Command.Point qualified as Point
import Quandary.Shell.Command.Prefer qualified as Prefer
import Quandary.Shell.Command.Reopen qualified as Reopen
import Quandary.Shell.Command.Rulings qualified as Rulings
import Quandary.Shell.Command.Settle qualified as Settle
import Quandary.Shell.Command.Show qualified as Show
import Quandary.Shell.Command.Status qualified as Status
import Quandary.Shell.Command.Whatif qualified as Whatif
import Quandary.Shell.Command.Why qualified as Why
import Quandary.Shell.Env (Env (..))
import Quandary.Shell.Exit (failWith)
import Quandary.Shell.Options (Action (..), Command (..), Options (..))
import Quandary.Shell.Options qualified as Options
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Storage.Files (Loaded (..))
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode, exitWith)
import System.IO (stderr)

main :: IO ()
main = Options.parse >>= run >>= exitWith

run :: Options -> IO ExitCode
run o = do
  loaded <- Files.readSpace o.dir
  let v = validate loaded.sources
      problems = loaded.problems ++ map (Files.locate o.dir) v.problems
  case o.command of
    Check -> Check.run o.dir problems v.model
    OnModel action -> do
      forM_ problems (Text.hPutStrLn stderr . Render.problem o.dir)
      case usableModel loaded v of
        Just m -> dispatch (Env o.dir o.point m) action
        Nothing -> failWith "the space has problems; nothing done"

usableModel :: Loaded -> Validation -> Maybe Model
usableModel loaded v
  | null loaded.problems = v.model
  | otherwise = Nothing

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
