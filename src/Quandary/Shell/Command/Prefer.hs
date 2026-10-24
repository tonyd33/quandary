module Quandary.Shell.Command.Prefer (run) where

import Data.Text (Text)
import Quandary.Core.Rulings
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> Text -> Text -> IO ExitCode
run env t n = Resolve.withOption env.model.space t $ \opt -> case rank n of
  Just r -> Rule.rule env [Prefer opt r]
  Nothing -> failWith ("rank " <> n <> ": expected an integer from 0 to 10, or -")

-- | Returns the rank written, Nothing for a cleared one.
rank :: Text -> Maybe (Maybe Int)
rank "-" = Just Nothing
rank n = Just <$> parseRank n
