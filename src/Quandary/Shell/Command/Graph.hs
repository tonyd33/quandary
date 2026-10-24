module Quandary.Shell.Command.Graph (run) where

import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Standing (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Render.Mermaid qualified as Mermaid
import System.Exit (ExitCode (..))

run :: Env -> IO ExitCode
run env = do
  a <- analysis env
  Text.putStr (Mermaid.graph env.model.space ((.verdicts) <$> Analyse.standing a))
  pure ExitSuccess
