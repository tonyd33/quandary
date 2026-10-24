module Quandary.Shell.Command.Whatif (run) where

import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = case whatifEdits env.model.space args of
  Left err -> failWith err
  Right es ->
    Analyse.whatif Cadical.cadical env.model env.point es >>= \case
      Left e -> failLines (Render.editError e)
      Right (m', mv) -> do
        raises <- maybe (pure []) (raisedLines m' env.point (accepted es)) (Analyse.standing mv.after)
        ExitSuccess <$ Text.putStr (Text.unlines (Render.movedLines m' env.point mv ++ raises))

raisedLines :: Model -> Text -> Set OptionId -> Analyse.Standing -> IO [Text]
raisedLines m name os s = Render.implicationLines m name s <$> Analyse.implications Cadical.cadical m name s os

accepted :: [Edit] -> Set OptionId
accepted es = Set.fromList [o | Accept o _ <- es]

whatifEdits :: Space -> [Text] -> Either Text [Edit]
whatifEdits _ [] = Right []
whatifEdits space (t : ts) = case verbEdit space t of
  Nothing -> Left ("whatif: expected accept, defeat or reopen before " <> t)
  Just edit -> (<>) <$> traverse (editOf edit) ids <*> whatifEdits space rest
  where
    (ids, rest) = break (isJust . verbEdit space) ts

verbEdit :: Space -> Text -> Maybe (Text -> Maybe Edit)
verbEdit space "accept" = Just (fmap (`Accept` Nothing) . resolveOption space)
verbEdit space "defeat" = Just (fmap (`Defeat` "") . resolveOption space)
verbEdit space "reopen" = Just (Resolve.reopening space)
verbEdit _ _ = Nothing

editOf :: (Text -> Maybe Edit) -> Text -> Either Text Edit
editOf edit t = maybe (Left ("unknown id " <> t <> ": whatif takes verbs and ids, no reasons")) Right (edit t)
