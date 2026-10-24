module Quandary.Shell.Command.Check (run) where

import Control.Monad (forM_)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Source (Document (..))
import Quandary.Core.Validate
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import Quandary.Shell.Storage.Files (FileProblem)
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

run :: FilePath -> [FileProblem] -> Maybe Model -> IO ExitCode
run dir problems model = do
  forM_ problems (Text.putStrLn . Render.problem dir)
  staleNames <- maybe (pure Set.empty) (Analyse.everyStale Cadical.cadical) model
  forM_ staleNames (Text.putStrLn . staleLine dir)
  if null problems && null staleNames
    then ExitSuccess <$ Text.putStrLn "ok"
    else pure (ExitFailure 1)

staleLine :: FilePath -> Text -> Text
staleLine dir n = Text.pack (Files.pathOf dir (PointDoc n)) <> ": the point is stale"
