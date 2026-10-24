module Quandary.Shell.Command.Whatif (run) where

import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Supposition (..))
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
run env args = case suppositions env.model.space args of
  Left err -> failWith err
  Right ss ->
    Analyse.whatif Cadical.cadical env.model env.point ss >>= \case
      Left e -> failLines (Render.editError e)
      Right (m', mv) -> do
        raises <- maybe (pure []) (raisedLines m' env.point (accepted ss)) (Analyse.standing mv.after)
        ExitSuccess <$ Text.putStr (Text.unlines (Render.movedLines m' env.point mv ++ raises))

raisedLines :: Model -> Text -> Set OptionId -> Analyse.Standing -> IO [Text]
raisedLines m name os s = Render.implicationLines m name s <$> Analyse.implications Cadical.cadical m name s os

accepted :: [Supposition] -> Set OptionId
accepted ss = Set.fromList [o | Suppose (Take o) <- ss]

suppositions :: Space -> [Text] -> Either Text [Supposition]
suppositions _ [] = Right []
suppositions space (t : ts) = case verbSupposition space t of
  Nothing -> Left ("whatif: expected accept, defeat or reopen before " <> t)
  Just resolve -> (<>) <$> traverse (supposition resolve) ids <*> suppositions space rest
  where
    (ids, rest) = break (isJust . verbSupposition space) ts

verbSupposition :: Space -> Text -> Maybe (Text -> Maybe Supposition)
verbSupposition space "accept" = Just (fmap (Suppose . Take) . resolveOption space)
verbSupposition space "defeat" = Just (fmap (Suppose . Refuse) . resolveOption space)
verbSupposition space "reopen" = Just (fmap Apply . Resolve.reopening space)
verbSupposition _ _ = Nothing

supposition :: (Text -> Maybe Supposition) -> Text -> Either Text Supposition
supposition resolve t = maybe (Left ("unknown id " <> t <> ": whatif takes verbs and ids, no reasons")) Right (resolve t)
