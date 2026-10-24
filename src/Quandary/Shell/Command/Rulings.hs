module Quandary.Shell.Command.Rulings (run) where

import Data.Text.IO qualified as Text
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import System.Exit (ExitCode (..))

run :: Env -> IO ExitCode
run env = ExitSuccess <$ Text.putStr (Render.rulings env.model env.point)
