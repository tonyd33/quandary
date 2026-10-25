module Quandary.Shell.Render.Dot (graph) where

import Data.Foldable (toList)
import Data.List.NonEmpty ((<|))
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Types
import Quandary.Shell.Render.Graph
import Quandary.Shell.Render.Text qualified as Render

-- | Draw the space as a Graphviz digraph: each question a cluster holding
-- its options coloured by verdict, then needs, excludes, raising and forbid
-- entries.
graph :: Graph -> Text
graph g =
  Text.unlines $
    ["digraph space {", "  rankdir=LR", "  compound=true", "  node [shape=box, style=rounded]"]
      ++ concatMap cluster g.questions
      ++ concatMap relations g.questions
      ++ concatMap raisers g.questions
      ++ concat (zipWith forbidNodes [0 :: Int ..] g.forbids)
      ++ ["}"]

cluster :: QuestionNode -> [Text]
cluster q =
  ["  subgraph " <> clusterName q.id <> " {", "    label=" <> quote q.label]
    ++ map optionDecl q.options
    ++ ["  }"]

optionDecl :: OptionNode -> Text
optionDecl o = "    " <> optionNode o.id <> attributes (("label", quote o.label) : maybe [] filled o.verdict)

relations :: QuestionNode -> [Text]
relations q =
  [ "  " <> optionNode o.id <> " -> " <> optionNode e.target <> attributes (relation e.relation)
  | o <- q.options
  , e <- o.edges
  ]

raisers :: QuestionNode -> [Text]
raisers q = concat (zipWith (raiser q.id) [0 :: Int ..] q.raises)

raiser :: Id -> Int -> Raise -> [Text]
raiser q _ (RaisedBy r) = ["  " <> optionNode r <> " -> " <> into q]
raiser q n (Joint r rs) =
  [ "  " <> joint <> attributes [("shape", "circle"), ("label", quote "all of")]
  , "  " <> joint <> " -> " <> into q
  ]
    ++ ["  " <> optionNode o <> " -> " <> joint <> attributes [("dir", "none")] | o <- toList (r <| rs)]
  where
    joint = quote ("joint " <> idText q <> " " <> Render.tshow n)

into :: Id -> Text
into q = optionNode (Other q) <> attributes [("lhead", clusterName q), ("penwidth", "2")]

forbidNodes :: Int -> ForbidNode -> [Text]
forbidNodes n f =
  ["  " <> forbid <> attributes [("shape", "hexagon"), ("label", quote f.label)]]
    ++ [ "  "
           <> forbid
           <> " -> "
           <> optionNode (optionOf l)
           <> attributes [("style", "dotted"), ("dir", "none"), ("label", side l)]
       | l <- f.literals
       ]
  where
    forbid = quote ("forbid " <> Render.tshow n)

filled :: Verdict -> [(Text, Text)]
filled v =
  [("style", quote "rounded,filled"), ("fillcolor", quote c.fill), ("color", quote c.stroke)]
    ++ maybe [] (\f -> [("fontcolor", quote f)]) c.font
  where
    c = colours v

relation :: Relation -> [(Text, Text)]
relation Needs = [("style", "dashed"), ("label", quote "needs")]
relation NeedsOneOf = [("style", "dashed"), ("label", quote "needs one of")]
relation Excludes = [("style", "dashed"), ("dir", "both"), ("arrowhead", "tee"), ("arrowtail", "tee"), ("label", quote "excludes")]

side :: Literal -> Text
side (Take _) = quote "with"
side (Refuse _) = quote "without"

attributes :: [(Text, Text)] -> Text
attributes as = " [" <> Text.intercalate ", " [k <> "=" <> v | (k, v) <- as] <> "]"

clusterName :: Id -> Text
clusterName q = quote ("cluster_" <> idText q)

optionNode :: OptionId -> Text
optionNode = quote . optionName

quote :: Text -> Text
quote t = "\"" <> Text.concatMap escape t <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape c = Text.singleton c
