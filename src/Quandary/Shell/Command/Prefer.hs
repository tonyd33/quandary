module Quandary.Shell.Command.Prefer (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> Text -> Text -> IO ExitCode
run env t n = Resolve.withOption env.model.space t $ \opt -> case writtenRank n of
  Just r -> Rule.rule env [Rule (Prefer opt r)]
  Nothing -> failWith (Render.badRank n <> ", or -")

-- | Returns the rank written, Nothing for a cleared one.
writtenRank :: Text -> Maybe (Maybe Rank)
writtenRank "-" = Just Nothing
writtenRank n = Just <$> parseRank n
