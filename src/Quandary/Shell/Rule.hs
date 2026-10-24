module Quandary.Shell.Rule (Group (..), rule, ruleGroups) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Analysis, Moved (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Env
import Quandary.Shell.Exit
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

data Group = Group
  { heading :: Text
  , edits :: [Edit]
  }

-- | One edit's model before and after, and what moved between them.
data Step = Step
  { before :: Model
  , after :: Model
  , moved :: Moved
  }

rule :: Env -> [Edit] -> IO ExitCode
rule env es = ruleGroups env [const (pure (Group (Render.ruled e) [e])) | e <- es]

-- | Each group receives the analysis before it. The first group's runs the
-- solver only when forced.
ruleGroups :: Env -> [IO Analysis -> IO Group] -> IO ExitCode
ruleGroups env groups = go env.model (analysis env) [] groups
  where
    go now _ done [] = do
      Files.writePoint env.dir (pointNamed now.points env.point)
      Text.putStr (Text.unlines (concat (reverse done)))
      pure ExitSuccess
    go now analysed done (group : rest) = do
      g <- group analysed
      Analyse.edit Cadical.cadical now env.point g.edits >>= \case
        Left r -> failLines (Render.refusal now env.point r <> abandoned)
        Right (next, mv) -> do
          body <- groupBody env (Step now next mv) g.edits
          go next (pure mv.after) ((g.heading : map ("  " <>) body) : done) rest
    abandoned
      | length groups > 1 = "nothing ruled\n"
      | otherwise = ""

groupBody :: Env -> Step -> [Edit] -> IO [Text]
groupBody env step es = do
  rs <- concat <$> traverse (remarks env step) es
  pure (movedLines env.point step es ++ concatMap (Render.remark step.before step.after env.point) rs)

movedLines :: Text -> Step -> [Edit] -> [Text]
movedLines point step es
  | not (null mv.options) && null others && Analyse.onlyOptionsMoved mv = []
  | otherwise = Render.movedLines step.after point mv {Analyse.options = others}
  where
    mv = step.moved
    ruled = mapMaybe ruledLiteral es
    others = filter (\o -> not (isRuled o || isImplied o)) mv.options
    isRuled (o, _, Forced) = Take o `elem` ruled
    isRuled (o, _, Impossible) = Refuse o `elem` ruled
    isRuled _ = False
    isImplied (Other q, _, Impossible) =
      q `elem` [x | (x, _, _) <- mv.questions] || Just q `elem` map (questionOf step.after.space . optionOf) ruled
    isImplied _ = False

remarks :: Env -> Step -> Edit -> IO [Render.Remark]
remarks env step (Accept opt r) = (++ committedRaisers env step opt) <$> ruledAgain env step (Take opt) r
remarks env step (Defeat opt r) = ruledAgain env step (Refuse opt) (Just r)
remarks env step (Reopen opt) = stillDecided env step opt
remarks _ step (Prefer opt (Just _)) = pure [Render.ImpossibleHere opt | Analyse.verdictAt step.moved.before opt == Just Impossible]
remarks _ _ _ = pure []

ruledAgain :: Env -> Step -> Literal -> Maybe Text -> IO [Render.Remark]
ruledAgain env step l r = maybe (alreadyDecided env step l) (pure . pure) (priorRuling step.before.points env.point l r)

priorRuling :: Map Text Point -> Text -> Literal -> Maybe Text -> Maybe Render.Remark
priorRuling points name l r = rulingNote <$> rulingFrom points name l
  where
    rulingNote (p, old)
      | p == name = Render.ReasonChanged old.reason r
      | otherwise = Render.AlsoRuledIn p

alreadyDecided :: Env -> Step -> Literal -> IO [Render.Remark]
alreadyDecided env step l
  | Analyse.verdictAt step.moved.before opt == Just v = pure . Render.AlreadyDecided opt v <$> causesAt env step.before opt v
  | otherwise = pure []
  where
    opt = optionOf l
    v = assertedVerdict l

stillDecided :: Env -> Step -> OptionId -> IO [Render.Remark]
stillDecided env step opt = case Analyse.verdictAt step.moved.after opt of
  Just v | v /= Possible -> pure . Render.StillDecided opt v <$> causesAt env step.after opt v
  _ -> pure []

committedRaisers :: Env -> Step -> OptionId -> [Render.Remark]
committedRaisers env step opt =
  [ Render.CommitsToRaiser r
  | question <- maybeToList (questionContaining step.before.space opt)
  , r <- concatMap toList question.raisedBy
  , Analyse.verdictAt step.moved.before r /= Just Forced
  , Analyse.verdictAt step.moved.after r == Just Forced
  , isNothing (rulingFrom step.after.points env.point (Take r))
  ]

causesAt :: Env -> Model -> OptionId -> Verdict -> IO [Set Literal]
causesAt env m = causes env (rulingsOf m.points env.point)
