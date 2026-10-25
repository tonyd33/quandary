module Quandary.Shell.Command.Graph (run) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Options (Format (..))
import Quandary.Shell.Render.Dot qualified as Dot
import Quandary.Shell.Render.Graph (Graph)
import Quandary.Shell.Render.Graph qualified as Graph
import Quandary.Shell.Render.Mermaid qualified as Mermaid
import System.Exit (ExitCode (..))

run :: Env -> Format -> IO ExitCode
run env format = do
  a <- analysis env
  Text.putStr (render format (Graph.build env.model.space (Analyse.standing a)))
  pure ExitSuccess

render :: Format -> Graph -> Text
render Mermaid = Mermaid.graph
render Dot = Dot.graph
