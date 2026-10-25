module Quandary.Shell.Command.Next (run) where

import Data.Text.IO qualified as Text
import Quandary.Core.Next qualified as Next
import Quandary.Core.Rulings (preferenceAt)
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> Int -> IO ExitCode
run env n = withFresh env env.point $ \s -> do
  ls <- Next.leads Cadical.cadical env.model env.point s n
  Text.putStr (Render.leads env.model.space (preferenceAt env.model.points env.point) ls)
  pure ExitSuccess
