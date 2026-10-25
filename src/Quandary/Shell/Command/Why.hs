module Quandary.Shell.Command.Why (run) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Standing (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> Text -> IO ExitCode
run env t = Resolve.withOption env.model.space t $ \opt ->
  withFresh env env.point $ \s -> do
    let verdict = s.verdicts Map.! opt
    cores <- Analyse.causes Cadical.cadical env.model env.point opt verdict
    Text.putStr (Render.why env.model env.point opt verdict cores)
    pure ExitSuccess
