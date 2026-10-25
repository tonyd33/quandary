module Quandary.Shell.Command.Adopt (run) where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (AdoptError (..), Adoption (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

run :: Env -> Text -> Analyse.Confirmation -> IO ExitCode
run env name confirmation =
  Analyse.adopt Cadical.cadical env.model name confirmation >>= \case
    Left UnknownBranch -> failWith ("unknown point " <> name)
    Left Proposed -> failWith (name <> " is proposed: confirm its rulings with the user, then adopt --confirmed")
    Left ExtendsNothing -> failWith (name <> " extends no point")
    Left (StaleBranch cores) -> reportStale env name cores
    Right a -> record env name a

record :: Env -> Text -> Adoption -> IO ExitCode
record env name a = do
  traverse_ (Files.writePoint env.dir) (changedPoints env.model a.model)
  Files.removePoint env.dir name
  Files.recordOverride env.dir env.override [adoptedLine name a]
  Text.putStr (adoptedText name a)
  pure ExitSuccess

changedPoints :: Model -> Model -> [Point]
changedPoints before after = filter changed (Map.elems after.points)
  where
    changed p = Map.lookup p.name before.points /= Just p

adoptedText :: Text -> Adoption -> Text
adoptedText name a =
  Text.unlines $
    [adoptedLine name a]
      ++ map ("  " <>) (Render.movedLines a.model a.into a.moved)
      ++ map droppedLine a.dropped
      ++ ["removed point " <> name]

adoptedLine :: Text -> Adoption -> Text
adoptedLine name a = "adopted " <> name <> " into " <> a.into

droppedLine :: (Text, [OptionId]) -> Text
droppedLine (p, os) = "  point " <> p <> " drops " <> Text.intercalate ", " (map optionName os)
