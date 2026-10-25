module Quandary.Shell.Command.Show (run) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Analysis)
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Core.View qualified as View
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> Text -> IO ExitCode
run env t = Resolve.withQuestionOrOption env.model.space t $ \found -> do
  a <- analysis env
  Text.putStr =<< shown env a found
  pure (exitCode a)

-- | Returns the question in full, or the option in full.
shown :: Env -> Analysis -> Either Question (Question, OptionId) -> IO Text
shown env a (Left q) = Render.showQuestion env.model.space <$> View.questionView Cadical.cadical env.model env.point a q
shown env a (Right (q, o)) = Render.showOption env.model <$> View.optionView Cadical.cadical env.model env.point a q o
