module Quandary.Shell.Command.Diff (run) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import System.Exit (ExitCode (..))

run :: Env -> Text -> Text -> IO ExitCode
run env a b =
  withFresh env a $ \sa ->
    withFresh env b $ \sb ->
      ExitSuccess <$ Text.putStr (Render.diff env.model (a, sa) (b, sb))
