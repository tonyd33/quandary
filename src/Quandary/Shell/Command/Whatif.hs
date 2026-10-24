module Quandary.Shell.Command.Whatif (run) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Supposition)
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Core.View qualified as View
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical qualified as Cadical
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = case suppositions env.model.space args of
  Left err -> failWith err
  Right ss -> do
    w <- View.whatifFacts Cadical.cadical env.model env.point ss
    case Render.whatifResult env.point w of
      Left err -> failLines err
      Right ls -> ExitSuccess <$ Text.putStr (Text.unlines ls)

suppositions :: Space -> [Text] -> Either Text [Supposition]
suppositions _ [] = Right []
suppositions space (t : ts) = case Resolve.verbNamed t of
  Nothing -> Left ("whatif: expected accept, defeat or reopen before " <> t)
  Just verb -> (<>) <$> traverse (supposition (Resolve.supposing space verb)) ids <*> suppositions space rest
  where
    (ids, rest) = break (isJust . Resolve.verbNamed) ts

supposition :: (Text -> Maybe Supposition) -> Text -> Either Text Supposition
supposition resolve t = maybe (Left ("unknown id " <> t <> ": whatif takes verbs and ids, no reasons")) Right (resolve t)
