module Quandary.Shell.Rule (rule) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Step (..))
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
  Analyse.edit Cadical.cadical env.model env.point es >>= \case
    Left (m, r) -> failLines (Render.refusal m env.point r <> abandoned)
    Right steps -> do
      Files.writePoint env.dir (pointNamed (last (env.model : map (.model) steps)).points env.point)
      Text.putStr (Text.unlines (concatMap (Render.stepLines env.point) steps))
      pure ExitSuccess
  where
    abandoned
      | length es > 1 = "nothing ruled\n"
      | otherwise = ""
