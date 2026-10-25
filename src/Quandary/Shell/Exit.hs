module Quandary.Shell.Exit (failWith, failLines, warn, exitCode) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import System.Exit (ExitCode (..))
import System.IO (stderr)

failWith :: Text -> IO ExitCode
failWith t = failLines (t <> "\n")

failLines :: Text -> IO ExitCode
failLines t = ExitFailure 1 <$ Text.hPutStr stderr ("quandary: " <> t)

warn :: Text -> IO ()
warn t = Text.hPutStrLn stderr ("quandary: warning: " <> t)

exitCode :: Analyse.Analysis -> ExitCode
exitCode a = if Analyse.stale a then ExitFailure 1 else ExitSuccess
