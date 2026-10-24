module Quandary.Shell.Command.Settle (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = Resolve.withReason "settle" (Resolve.questionsThenReason env.model.space args) $ \qs reason ->
  Rule.rule env [Settle q reason | q <- qs]
