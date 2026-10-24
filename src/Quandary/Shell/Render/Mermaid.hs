module Quandary.Shell.Render.Mermaid (graph) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Types
import Quandary.Shell.Render.Text qualified as Render

-- | Draw the space as a flowchart: questions, their options coloured by
-- verdict, raising, needs, excludes and forbid entries.
graph :: Space -> Maybe (Map OptionId Verdict) -> Text
graph space verdicts =
  Text.unlines $
    ["flowchart LR"]
      ++ concat [questionNodes q | q <- qs]
      ++ concat [forbidNodes n f | (n, f) <- zip [0 :: Int ..] (concatMap (.forbid) qs)]
      ++ [ "  classDef forced fill:#b7e4b7,stroke:#2e7d32"
         , "  classDef possible fill:#fff3c4,stroke:#b8860b"
         , "  classDef impossible fill:#e0e0e0,stroke:#9e9e9e,color:#757575"
         ]
      ++ [ "  class " <> optionNode o <> " " <> Render.verdictText v
         | Just vs <- [verdicts]
         , (o, v) <- Map.toList vs
         ]
  where
    qs = Map.elems space.questions
    questionNodes q =
      ["  " <> questionNode q.id <> "[\"" <> label (idText q.id <> ": " <> q.title) <> "\"]"]
        ++ concat
          [ [ "  " <> optionNode (Listed o.id) <> "(\"" <> label (idText o.id <> ": " <> o.title) <> "\")"
            , "  " <> questionNode q.id <> " --> " <> optionNode (Listed o.id)
            ]
              ++ ["  " <> optionNode (Listed o.id) <> " -. needs .-> " <> optionNode t | t <- o.needs]
              ++ ["  " <> optionNode (Listed o.id) <> " -. needs one of .-> " <> optionNode t | t <- maybe [] toList o.needsOneOf]
              ++ ["  " <> optionNode (Listed o.id) <> " x-. excludes .-x " <> optionNode t | t <- o.excludes]
          | o <- q.options
          ]
        ++ [ "  " <> optionNode (Other q.id) <> "(\"" <> label (idText q.id <> ".other") <> "\")"
           , "  " <> questionNode q.id <> " --> " <> optionNode (Other q.id)
           ]
        ++ concat (zipWith (raiser q.id) [0 :: Int ..] q.raisedBy)
    raiser q _ (r :| []) = ["  " <> optionNode r <> " ==> " <> questionNode q]
    raiser q n rs =
      ["  " <> joint <> "((\"all of\"))", "  " <> joint <> " ==> " <> questionNode q]
        ++ ["  " <> optionNode r <> " --- " <> joint | r <- toList rs]
      where
        joint = "j" <> Render.tshow n <> "_" <> questionNode q
    forbidNodes n f =
      ["  f" <> Render.tshow n <> "{{\"" <> label ("forbid" <> maybe "" (": " <>) f.why) <> "\"}}"]
        ++ [ "  f" <> Render.tshow n <> " -. " <> (case l of Take _ -> "with"; Refuse _ -> "without") <> " .- " <> optionNode (optionOf l)
           | l <- Set.toList f.literals
           ]

questionNode :: Id -> Text
questionNode (Id t) = "q_" <> safe t

optionNode :: OptionId -> Text
optionNode (Listed (Id t)) = "o_" <> safe t
optionNode (Other (Id t)) = "x_" <> safe t

-- | Mermaid ids take letters, digits and underscores; other characters are
-- spelled as their code points.
safe :: Text -> Text
safe = Text.concatMap (\c -> if isAsciiLower c || isAsciiUpper c || isDigit c then Text.singleton c else "_" <> Render.tshow (fromEnum c) <> "_")

label :: Text -> Text
label = Text.replace "\"" "#quot;"
