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
run env t = Resolve.withQuestionOrOption env.model.space t $ \case
  Left question -> do
    a <- analysis env
    v <- View.questionView Cadical.cadical env.model env.point a question
    Text.putStr (Render.showQuestion env.model.space v)
    pure (exitCode a)
  Right (question, o) -> do
    a <- analysis env
    v <- View.optionView Cadical.cadical env.model env.point a question o
    Text.putStr (Render.showOption env.model v)
    pure (exitCode a)
