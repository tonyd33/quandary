module Quandary.Shell.Command.Show (run) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> Text -> IO ExitCode
run env q = Resolve.withQuestion env.model.space q $ \question -> do
  a <- analysis env
  bs <- case Analyse.standing a of
    Nothing -> pure Map.empty
    Just s -> Analyse.bounds Cadical.cadical env.model env.point s (choices question)
  Text.putStr (Render.showQuestion env.model env.point a bs (Analyse.dominance env.model env.point bs) question)
  pure (exitCode a)
