module Quandary.Shell.Rule (rule, ruled) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Refusal, Step (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

rule :: Env -> [Edit] -> IO ExitCode
rule env es =
  ruled env.dir env.model env.point es >>= \case
    Left (m, r) -> failLines (Render.refusal m env.point r <> abandoned)
    Right steps -> ExitSuccess <$ Text.putStr (Text.unlines (concatMap (Render.stepLines env.point) steps))
  where
    abandoned
      | length es > 1 = "nothing ruled\n"
      | otherwise = ""

-- | Apply the edits to the named point and write it, returning each step.
-- When they would leave the point stale, write nothing and return the
-- refusal with the model it applies to.
ruled :: FilePath -> Model -> Text -> [Edit] -> IO (Either (Model, Refusal) [Step])
ruled dir m name es =
  Analyse.edit Cadical.cadical m name es >>= \case
    Left refused -> pure (Left refused)
    Right steps -> do
      Files.writePoint dir (pointNamed (last (m : map (.model) steps)).points name)
      pure (Right steps)
