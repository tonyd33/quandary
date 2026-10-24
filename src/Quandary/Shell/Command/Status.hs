module Quandary.Shell.Command.Status (run) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Standing (..))
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
  bs <- case Analyse.standing a of
    Nothing -> pure Map.empty
    Just s -> Analyse.bounds Cadical.cadical env.model env.point s (openOptions env.model.space s)
  staleNames <- Analyse.everyStale Cadical.cadical env.model
  Text.putStr (Render.status env.model env.point a bs (Just staleNames))
  pure (exitCode a)

openOptions :: Space -> Standing -> [OptionId]
openOptions space s = [opt | q <- Map.elems space.questions, Analyse.isOpen (s.states Map.! q.id), opt <- choices q]
