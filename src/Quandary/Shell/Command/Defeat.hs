module Quandary.Shell.Command.Defeat (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = Resolve.withReason "defeat" (Resolve.optionsThenReason env.model.space args) $ \os reason ->
  Rule.rule env [Rule (Defeat opt reason) | opt <- os]
