module Quandary.Shell.Command.Settle (run) where

import Data.Text (Text)
import Quandary.Core.Analyse (Analysis (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Rule (Group (..))
import Quandary.Shell.Rule qualified as Rule
import System.Exit (ExitCode (..))

run :: Env -> [Text] -> IO ExitCode
run env args = Resolve.withReason "settle" (Resolve.questionsThenReason env.model.space args) $ \qs reason ->
  Rule.ruleGroups env (map (settling reason) qs)

settling :: Text -> Question -> IO Analysis -> IO Group
settling reason question analysed = do
  a <- analysed
  let open = filter ((== Just Possible) . Analyse.verdictAt a) (listedThenOther question)
  pure (Group (Render.settled question.id open) [Defeat o reason | o <- open])
