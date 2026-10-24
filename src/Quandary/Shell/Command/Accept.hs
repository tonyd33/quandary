module Quandary.Shell.Command.Accept (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = case Resolve.optionsThenReason env.model.space args of
  Left err -> failWith err
  Right (os, reason) -> Rule.rule env [Accept opt reason | opt <- os]
