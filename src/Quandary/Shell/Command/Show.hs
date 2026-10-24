module Quandary.Shell.Command.Show (run) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Validate
import Quandary.Core.View qualified as View
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> Text -> IO ExitCode
run env q = Resolve.withQuestion env.model.space q $ \question -> do
  a <- analysis env
  v <- View.questionView Cadical.cadical env.model env.point a question
  Text.putStr (Render.showQuestion env.model.space v)
  pure (exitCode a)
