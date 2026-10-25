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
  , shown
  , raise
  , edgesOf
  , forbidLabel
  , colours
  )
where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Analyse (QuestionState, Standing (..))
import Quandary.Core.Compile qualified as Compile
import Quandary.Core.Types
import Quandary.Core.View (Dead, hidesOption, hidesQuestion)

data Graph = Graph
  { questions :: [QuestionNode]
  , forbids :: [ForbidNode]
  }

data QuestionNode = QuestionNode
  { id :: Id
  , label :: Text
  , state :: Maybe QuestionState
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
  deriving (Eq, Ord)

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

build :: Space -> Maybe Standing -> Graph
build space standing =
  Graph
    { questions = map (questionNode standing edges) qs
    , forbids = map forbidNode (concatMap (.forbid) qs)
    }
  where
    qs = Map.elems space.questions
    edges =
      Map.map
        (sortOn (.relation))
        (Map.fromListWith (flip (++)) [(o, [e]) | (c, _) <- Compile.constraints space, (o, e) <- edgesOf c])

-- | Returns the edges the constraint draws, each with the option it leaves.
edgesOf :: Compile.Constraint -> [(OptionId, Edge)]
edgesOf (Compile.Needs o p) = [(o, Edge Needs p)]
edgesOf (Compile.NeedsOneOf o ps) = [(o, Edge NeedsOneOf p) | p <- toList ps]
edgesOf (Compile.Excludes o p) = [(o, Edge Excludes p)]
edgesOf _ = []

questionNode :: Maybe Standing -> Map OptionId [Edge] -> Question -> QuestionNode
questionNode standing edges q =
  QuestionNode
    { id = q.id
    , label = idText q.id <> ": " <> q.title
    , state = Map.lookup q.id . (.states) =<< standing
    , options =
        [node (Listed o.id) (idText o.id <> ": " <> o.title) | o <- q.options] ++ [node (Other q.id) (optionName (Other q.id))]
    , raises = map raise q.raisedBy
    }
  where
    verdictOf o = Map.lookup o . (.verdicts) =<< standing
    node o label = OptionNode o label (verdictOf o) (Map.findWithDefault [] o edges)

-- | Returns the graph with the questions and options the view shows,
-- dropping each raise and edge from or to an option left out.
shown :: Dead -> Graph -> Graph
shown dead g = Graph [shownQuestion q | q <- g.questions, not (hidden q)] g.forbids
  where
    hidden q = hidesQuestion dead q.state
    hiddenOptions = Set.fromList [o.id | q <- g.questions, o <- q.options, hidden q || hidesOption dead o.verdict]
    kept o = Set.notMember o hiddenOptions
    shownQuestion q =
      QuestionNode
        { id = q.id
        , label = q.label
        , state = q.state
        , options = [OptionNode o.id o.label o.verdict (filter (kept . (.target)) o.edges) | o <- q.options, kept o.id]
        , raises = filter (all kept . raisers) q.raises
        }

raisers :: Raise -> NonEmpty OptionId
raisers (RaisedBy o) = o :| []
raisers (Joint o ps) = o :| toList ps

raise :: NonEmpty OptionId -> Raise
raise (r :| []) = RaisedBy r
raise (r :| s : rs) = Joint r (s :| rs)

forbidNode :: Forbid -> ForbidNode
forbidNode f = ForbidNode {label = forbidLabel f, literals = Set.toList f.literals}

forbidLabel :: Forbid -> Text
forbidLabel f = "forbid" <> maybe "" (": " <>) f.why

colours :: Verdict -> Colours
colours Forced = Colours "#b7e4b7" "#2e7d32" Nothing
colours Possible = Colours "#fff3c4" "#b8860b" Nothing
colours Impossible = Colours "#e0e0e0" "#9e9e9e" (Just "#757575")
