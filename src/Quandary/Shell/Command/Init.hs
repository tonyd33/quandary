module Quandary.Shell.Command.Init (run) where

import Control.Monad (forM_, when)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

run :: FilePath -> IO ExitCode
run dir = do
  created <- Files.createSpace dir
  forM_ created (\path -> Text.putStrLn ("created " <> Text.pack path))
  when (null created) (Text.putStrLn "already a space")
  pure ExitSuccess
