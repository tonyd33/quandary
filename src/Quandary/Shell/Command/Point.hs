module Quandary.Shell.Command.Point (create, remove) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

create :: Env -> Text -> Maybe Text -> Bool -> IO ExitCode
create env name parent proposed = case newPoint env.model.points name parent proposed of
  Left e -> failWith (Render.pointError e)
  Right p -> do
    Files.writePoint env.dir p
    Files.recordOverride env.dir env.override [createdLine p]
    Text.putStrLn (createdLine p)
    pure ExitSuccess

remove :: Env -> Text -> IO ExitCode
remove env name = case withoutPoint env.model.points name of
  Left e -> failWith (Render.pointError e)
  Right _ -> do
    Files.removePoint env.dir name
    Files.recordOverride env.dir env.override [removedLine name]
    Text.putStrLn (removedLine name)
    pure ExitSuccess

removedLine :: Text -> Text
removedLine name = "removed point " <> name

createdLine :: Point -> Text
createdLine p = "created " <> marker <> "point " <> p.name <> maybe "" (" extending " <>) p.extends
  where
    marker = if p.proposed then "proposed " else ""
