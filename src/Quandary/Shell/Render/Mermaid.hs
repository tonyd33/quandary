module Quandary.Shell.Render.Mermaid (graph) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty ((<|))
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Types
import Quandary.Shell.Render.Graph
import Quandary.Shell.Render.Text qualified as Render

-- | Draw the space as a flowchart: questions, their options coloured by
-- verdict, raising, needs, excludes and forbid entries.
graph :: Graph -> Text
graph g =
  Text.unlines $
    ["flowchart LR"]
      ++ concatMap questionNodes g.questions
      ++ concat (zipWith forbidNodes [0 :: Int ..] g.forbids)
      ++ map classDef [Forced, Possible, Impossible]
      ++ map classLine (verdictsByOption g)

questionNodes :: QuestionNode -> [Text]
questionNodes q =
  ["  " <> questionNode q.id <> "[\"" <> label q.label <> "\"]"]
    ++ concatMap (optionNodes q.id) q.options
    ++ concat (zipWith (raiser q.id) [0 :: Int ..] q.raises)

optionNodes :: Id -> OptionNode -> [Text]
optionNodes q o =
  [ "  " <> optionNode o.id <> "(\"" <> label o.label <> "\")"
  , "  " <> questionNode q <> " --> " <> optionNode o.id
  ]
    ++ ["  " <> optionNode o.id <> " " <> relation e.relation <> " " <> optionNode e.target | e <- o.edges]

raiser :: Id -> Int -> Raise -> [Text]
raiser q _ (RaisedBy r) = ["  " <> optionNode r <> " ==> " <> questionNode q]
raiser q n (Joint r rs) =
  ["  " <> joint <> "((\"all of\"))", "  " <> joint <> " ==> " <> questionNode q]
    ++ ["  " <> optionNode o <> " --- " <> joint | o <- toList (r <| rs)]
  where
    joint = "j" <> Render.tshow n <> "_" <> questionNode q

forbidNodes :: Int -> ForbidNode -> [Text]
forbidNodes n f =
  ["  f" <> Render.tshow n <> "{{\"" <> label f.label <> "\"}}"]
    ++ ["  f" <> Render.tshow n <> " -. " <> side l <> " .- " <> optionNode (optionOf l) | l <- f.literals]

classDef :: Verdict -> Text
classDef v =
  "  classDef " <> Render.verdictText v <> " fill:" <> c.fill <> ",stroke:" <> c.stroke <> maybe "" (",color:" <>) c.font
  where
    c = colours v

classLine :: (OptionId, Verdict) -> Text
classLine (o, v) = "  class " <> optionNode o <> " " <> Render.verdictText v

verdictsByOption :: Graph -> [(OptionId, Verdict)]
verdictsByOption g = sortOn fst [(o.id, v) | q <- g.questions, o <- q.options, Just v <- [o.verdict]]

relation :: Relation -> Text
relation Needs = "-. needs .->"
relation NeedsOneOf = "-. needs one of .->"
relation Excludes = "x-. excludes .-x"

side :: Literal -> Text
side (Take _) = "with"
side (Refuse _) = "without"

questionNode :: Id -> Text
questionNode (Id t) = "q_" <> safe t

optionNode :: OptionId -> Text
optionNode (Listed (Id t)) = "o_" <> safe t
optionNode (Other (Id t)) = "x_" <> safe t

-- | Mermaid ids take letters, digits and underscores; other characters are
-- spelled as their code points.
safe :: Text -> Text
safe = Text.concatMap safeChar

safeChar :: Char -> Text
safeChar c
  | isAsciiLower c || isAsciiUpper c || isDigit c = Text.singleton c
  | otherwise = "_" <> Render.tshow (fromEnum c) <> "_"

label :: Text -> Text
label = Text.replace "\"" "#quot;"
