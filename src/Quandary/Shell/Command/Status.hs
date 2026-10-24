module Quandary.Shell.Command.Status (run) where

import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.View qualified as View
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> IO ExitCode
run env = do
  a <- analysis env
  bs <- View.statusBounds Cadical.cadical env.model env.point a
  staleNames <- Analyse.everyStale Cadical.cadical env.model
  Text.putStr (Render.status env.model env.point a bs (Just (View.pointViews env.model staleNames)))
  pure (exitCode a)
