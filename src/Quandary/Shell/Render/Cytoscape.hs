module Quandary.Shell.Render.Cytoscape
  ( elements
  , explanation
  , questionKey
  , optionKey
  , relationClass
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Analyse (Cause (..), QuestionState (..))
import Quandary.Core.Compile (Constraint (..))
import Quandary.Core.Types
import Quandary.Shell.Render.Graph
  ( Edge (..)
  , Graph (..)
  , OptionNode (..)
  , QuestionNode (..)
  , Raise (..)
  )
import Quandary.Shell.Render.Graph qualified as Graph
import Quandary.Shell.Render.Text qualified as Render

-- | Returns the graph's questions as compound nodes holding their options,
-- with an edge from each raiser to the question it raises and a joint node
-- for raisers that raise only together. Each question and option carries
-- the page it opens. Each option also carries its needs, needs-one-of and
-- excludes as pairs of class and target key.
elements :: (Id -> Text) -> (OptionId -> Text) -> Graph -> [Value]
elements questionHref optionHref g = concatMap question g.questions
  where
    question q =
      node (questionKey q.id) Nothing q.label (questionClass q.state) (questionHref q.id) []
        : map (option q) q.options
        ++ concat (zipWith (raiseElements q.id) [0 ..] q.raises)
    option q o =
      node
        (optionKey o.id)
        (Just (questionKey q.id))
        o.label
        (verdictClass o.verdict)
        (optionHref o.id)
        ["constraints" .= [[relationClass e.relation, optionKey e.target] | e <- o.edges]]

-- | Returns the edges from the raiser to the question, through a joint
-- node numbered n when its options raise it only together.
raiseElements :: Id -> Int -> Raise -> [Value]
raiseElements q _ (RaisedBy o) = [edge (optionKey o) (questionKey q) "raises"]
raiseElements q n (Joint o (p :| ps)) =
  object ["data" .= object ["id" .= joint, "label" .= ("all of" :: Text)], "classes" .= ("joint" :: Text)]
    : edge joint (questionKey q) "raises"
    : [edge (optionKey r) joint "raises" | r <- o : p : ps]
  where
    joint = "j:" <> idText q <> ":" <> Render.tshow n

-- | Returns the cause as a graph: its option and the options its rulings
-- and constraints name, each inside its question. Each question is
-- labelled with the pick and raising constraints on it. Edges run for each
-- needs, needs-one-of and excludes, from each raiser to a question raised
-- first, and from a node per forbid entry to its options. Each option
-- carries its verdict, its ruling in the cause and the page it opens.
explanation ::
  Space -> (OptionId -> Maybe Verdict) -> (Id -> Text) -> (OptionId -> Text) -> OptionId -> Cause -> [Value]
explanation space verdictOf questionHref optionHref target cause =
  map question (nubOrd (mapMaybe (questionOf space) options))
    ++ map option options
    ++ concatMap constraintElements cause.constraints
  where
    options = nubOrd (target : map optionOf (Set.toList cause.rulings) ++ concatMap constraintOptions cause.constraints)
    question q = node (questionKey q) Nothing (questionLabel q cause.constraints) "question" (questionHref q) []
    option o =
      node
        (optionKey o)
        (questionKey <$> questionOf space o)
        (rulingPrefix cause.rulings o <> Render.key o)
        (verdictClass (verdictOf o) <> targetClass target o)
        (optionHref o)
        []

targetClass :: OptionId -> OptionId -> Text
targetClass target o
  | o == target = " target"
  | otherwise = ""

-- | Returns the options the constraint names.
constraintOptions :: Constraint -> [OptionId]
constraintOptions (Needs o p) = [o, p]
constraintOptions (Excludes o p) = [o, p]
constraintOptions (NeedsOneOf o ps) = o : toList ps
constraintOptions (RaisedFirst _ os rs) = os ++ concatMap toList rs
constraintOptions (AtMostOne _ os) = os
constraintOptions (AtLeastOne _ os _) = os
constraintOptions (Forbidden _ f) = map optionOf (Set.toList f.literals)

constraintElements :: Constraint -> [Value]
constraintElements (Needs o p) = [edge (optionKey o) (optionKey p) "needs"]
constraintElements (Excludes o p) = [edge (optionKey o) (optionKey p) "excludes"]
constraintElements (NeedsOneOf o ps) = [edge (optionKey o) (optionKey p) "needs-one-of" | p <- toList ps]
constraintElements (RaisedFirst q _ rs) = concat (zipWith (raiseElements q) [0 ..] (map Graph.raise rs))
constraintElements (Forbidden q f) =
  object ["data" .= object ["id" .= key, "label" .= Graph.forbidLabel f], "classes" .= ("forbid" :: Text)]
    : [edge key (optionKey (optionOf l)) (forbidClass l) | l <- Set.toList f.literals]
  where
    key = "f:" <> idText q <> ":" <> Text.intercalate "," (map literalKey (Set.toList f.literals))
constraintElements _ = []

literalKey :: Literal -> Text
literalKey (Take o) = "+" <> optionName o
literalKey (Refuse o) = "-" <> optionName o

forbidClass :: Literal -> Text
forbidClass (Take _) = "forbids taking"
forbidClass (Refuse _) = "forbids refusing"

-- | Returns the question's id over the pick and raising constraints on it.
questionLabel :: Id -> [Constraint] -> Text
questionLabel q cs = Text.intercalate "\n" (idText q : [rule | c <- cs, Just (q', rule) <- [questionRule c], q' == q])

questionRule :: Constraint -> Maybe (Id, Text)
questionRule (RaisedFirst q _ _) = Just (q, "only once raised")
questionRule (AtMostOne q _) = Just (q, "at most one")
questionRule (AtLeastOne q _ _) = Just (q, "at least one")
questionRule _ = Nothing

rulingPrefix :: Set Literal -> OptionId -> Text
rulingPrefix rs o = case find ((== o) . optionOf) (Set.toList rs) of
  Just l -> Render.rulingVerb l <> " "
  Nothing -> ""

node :: Text -> Maybe Text -> Text -> Text -> Text -> [Pair] -> Value
node key parent label cls href more =
  object
    [ "data" .= object (["id" .= key, "label" .= label, "href" .= href] ++ ["parent" .= p | Just p <- [parent]] ++ more)
    , "classes" .= cls
    ]

edge :: Text -> Text -> Text -> Value
edge from to cls = object ["data" .= object ["source" .= from, "target" .= to], "classes" .= cls]

questionKey :: Id -> Text
questionKey q = "q:" <> idText q

optionKey :: OptionId -> Text
optionKey o = "o:" <> optionName o

questionClass :: Maybe QuestionState -> Text
questionClass (Just Dormant) = "question dormant"
questionClass _ = "question"

verdictClass :: Maybe Verdict -> Text
verdictClass = maybe "option" (("option " <>) . Render.verdictText)

relationClass :: Graph.Relation -> Text
relationClass Graph.Needs = "needs"
relationClass Graph.NeedsOneOf = "needs-one-of"
relationClass Graph.Excludes = "excludes"
