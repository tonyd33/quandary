module Quandary.Shell.Render.Graph
  ( Graph (..)
  , QuestionNode (..)
  , OptionNode (..)
  , Edge (..)
  , Relation (..)
  , Raise (..)
  , ForbidNode (..)
  , Colours (..)
  , build
  , colours
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Types

data Graph = Graph
  { questions :: [QuestionNode]
  , forbids :: [ForbidNode]
  }

data QuestionNode = QuestionNode
  { id :: Id
  , label :: Text
  , options :: [OptionNode]
  -- ^ Listed options in order, then `.other`.
  , raises :: [Raise]
  }

data OptionNode = OptionNode
  { id :: OptionId
  , label :: Text
  , verdict :: Maybe Verdict
  , edges :: [Edge]
  }

data Edge = Edge
  { relation :: Relation
  , target :: OptionId
  }

data Relation = Needs | NeedsOneOf | Excludes

data Raise
  = RaisedBy OptionId
  | Joint OptionId (NonEmpty OptionId)

data ForbidNode = ForbidNode
  { label :: Text
  , literals :: [Literal]
  }

data Colours = Colours
  { fill :: Text
  , stroke :: Text
  , font :: Maybe Text
  }

build :: Space -> Maybe (Map OptionId Verdict) -> Graph
build space verdicts =
  Graph
    { questions = map (questionNode verdictOf) qs
    , forbids = map forbidNode (concatMap (.forbid) qs)
    }
  where
    qs = Map.elems space.questions
    verdictOf o = verdicts >>= Map.lookup o

questionNode :: (OptionId -> Maybe Verdict) -> Question -> QuestionNode
questionNode verdictOf q =
  QuestionNode
    { id = q.id
    , label = idText q.id <> ": " <> q.title
    , options = map (listedNode verdictOf) q.options ++ [otherNode]
    , raises = map raise q.raisedBy
    }
  where
    otherNode = OptionNode (Other q.id) (optionName (Other q.id)) (verdictOf (Other q.id)) []

listedNode :: (OptionId -> Maybe Verdict) -> Option -> OptionNode
listedNode verdictOf o =
  OptionNode
    { id = Listed o.id
    , label = idText o.id <> ": " <> o.title
    , verdict = verdictOf (Listed o.id)
    , edges =
        map (Edge Needs) o.needs
          ++ map (Edge NeedsOneOf) (maybe [] toList o.needsOneOf)
          ++ map (Edge Excludes) o.excludes
    }

raise :: NonEmpty OptionId -> Raise
raise (r :| []) = RaisedBy r
raise (r :| s : rs) = Joint r (s :| rs)

forbidNode :: Forbid -> ForbidNode
forbidNode f = ForbidNode {label = "forbid" <> maybe "" (": " <>) f.why, literals = Set.toList f.literals}

colours :: Verdict -> Colours
colours Forced = Colours "#b7e4b7" "#2e7d32" Nothing
colours Possible = Colours "#fff3c4" "#b8860b" Nothing
colours Impossible = Colours "#e0e0e0" "#9e9e9e" (Just "#757575")
