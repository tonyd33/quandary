module Quandary.Shell.Render.Cytoscape
  ( elements
  , questionKey
  , optionKey
  )
where

import Data.Aeson (Value, object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Types
import Quandary.Shell.Render.Graph
  ( Edge (..)
  , Graph (..)
  , OptionNode (..)
  , QuestionNode (..)
  , Raise (..)
  , Relation (..)
  )

-- | Returns the graph's questions as compound nodes holding their options,
-- with an edge from each raiser to the question it raises, a joint node
-- for raisers that raise only together, and the options' needs and
-- excludes edges. Each question and option carries the page it opens.
elements :: (Id -> Text) -> (OptionId -> Text) -> Graph -> [Value]
elements questionHref optionHref g = concatMap question g.questions
  where
    question q =
      node (questionKey q.id) Nothing q.label "question" (questionHref q.id)
        : concatMap (option q) q.options
        ++ concat (zipWith (raise q) [0 :: Int ..] q.raises)
    option q o =
      node (optionKey o.id) (Just (questionKey q.id)) o.label (verdictClass o.verdict) (optionHref o.id)
        : [edge (optionKey o.id) (optionKey e.target) (relationClass e.relation) | e <- o.edges]
    raise q _ (RaisedBy o) = [edge (optionKey o) (questionKey q.id) "raises"]
    raise q n (Joint o (p :| ps)) =
      object ["data" .= object ["id" .= joint, "label" .= ("all of" :: Text)], "classes" .= ("joint" :: Text)]
        : edge joint (questionKey q.id) "raises"
        : [edge (optionKey r) joint "raises" | r <- o : p : ps]
      where
        joint = "j:" <> idText q.id <> ":" <> tshow n

node :: Text -> Maybe Text -> Text -> Text -> Text -> Value
node key parent label cls href =
  object
    [ "data" .= object (["id" .= key, "label" .= label, "href" .= href] ++ ["parent" .= p | Just p <- [parent]])
    , "classes" .= cls
    ]

edge :: Text -> Text -> Text -> Value
edge from to cls = object ["data" .= object ["source" .= from, "target" .= to], "classes" .= cls]

questionKey :: Id -> Text
questionKey q = "q:" <> idText q

optionKey :: OptionId -> Text
optionKey o = "o:" <> optionName o

verdictClass :: Maybe Verdict -> Text
verdictClass (Just Forced) = "option forced"
verdictClass (Just Possible) = "option possible"
verdictClass (Just Impossible) = "option impossible"
verdictClass Nothing = "option"

relationClass :: Relation -> Text
relationClass Needs = "needs"
relationClass NeedsOneOf = "needs-one-of"
relationClass Excludes = "excludes"

tshow :: Show a => a -> Text
tshow = Text.pack . show
