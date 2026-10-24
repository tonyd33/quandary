module Quandary.Shell.Command.Status (run) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Analysis (..), Standing (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> IO ExitCode
run env = do
  a <- analysis env
  bs <- Analyse.bounds Cadical.cadical env.model env.point a (openOptions env.model.space a)
  staleNames <- Analyse.everyStale Cadical.cadical env.model
  Text.putStr (Render.status env.model env.point a bs (Just staleNames))
  pure (exitCode a)

openOptions :: Space -> Analysis -> [OptionId]
openOptions _ (StaleAt _) = []
openOptions space (Fresh s) = [opt | q <- Map.elems space.questions, Map.lookup q.id s.states == Just Analyse.Open, opt <- choices q]
