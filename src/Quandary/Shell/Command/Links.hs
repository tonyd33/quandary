module Quandary.Shell.Command.Links (run) where

import Data.Text.IO qualified as Text
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import System.Exit (ExitCode (..))

run :: Env -> IO ExitCode
run env = ExitSuccess <$ Text.putStr (Render.links env.model.space)
