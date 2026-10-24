module Quandary.Shell.Command.Reopen (run) where

import Data.Text (Text)
import Quandary.Core.Rulings (Edit (..))
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env ts = either failWith (Rule.rule env . map Rule) (traverse reopened ts)
  where
    reopened t = maybe (Left ("unknown id " <> t)) Right (Resolve.reopening env.model.space t)
