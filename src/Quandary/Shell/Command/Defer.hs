module Quandary.Shell.Command.Defer (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> Text -> IO ExitCode
run env q = Resolve.withQuestion env.model.space q $ \question -> Rule.rule env [Defer question.id]
