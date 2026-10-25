module Quandary.Core.View
  ( QuestionView (..)
  , OptionRow (..)
  , OptionView (..)
  , Domination (..)
  , Objection (..)
  , RaisedOption (..)
  , PointView (..)
  , Group (..)
  , GroupKind (..)
  , Entry (..)
  , Detail (..)
  , Choices (..)
  , Choice (..)
  , Whatif (..)
  , questionView
  , questionViewFrom
  , optionView
  , statusBounds
  , statusGroups
  , choicesAt
  , pointViews
  , whatifFacts
  )
where

import Control.Monad (mfilter)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Analyse
import Quandary.Core.Order (decisionOrder)
import Quandary.Core.Rulings
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))

data QuestionView = QuestionView
  { point :: Text
  , question :: Question
  , analysis :: Analysis
  , rows :: [OptionRow]
  -- ^ By preference at the point.
  }

-- | An option of a question at a point.
data OptionRow = OptionRow
  { option :: OptionId
  , title :: Maybe Text
  , verdict :: Maybe Verdict
  , ruling :: Maybe (Literal, Text, Ruling)
  -- ^ The ruling on it with the point it comes from.
  , bounds :: Maybe Bounds
  , rank :: Maybe Rank
  , assessment :: Maybe Assessment
  , dominated :: Maybe Domination
  , raises :: [Raising]
  , objections :: [Objection]
  }
  deriving (Eq, Show)

-- | A sibling's lower bound above an option's upper bound.
data Domination = Domination
  { by :: OptionId
  , atBest :: Int
  , byAtWorst :: Int
  }
  deriving (Eq, Show)

-- | A question an option raises, with its options' verdicts and the
-- questions they raise in turn.
data Objection = Objection
  { raising :: Raising
  , state :: QuestionState
  , options :: [RaisedOption]
  }
  deriving (Eq, Show)

data RaisedOption = RaisedOption
  { option :: OptionId
  , verdict :: Verdict
  , objections :: [Objection]
  }
  deriving (Eq, Show)

data PointView = PointView
  { point :: Point
  , stale :: Bool
  , rulesNothingElse :: Bool
  -- ^ It extends a point and rules and defers just what that one does.
  }
  deriving (Eq, Show)

-- | Where status lists a question at a fresh point.
data GroupKind = Frontier | TakenMore | RaisedIfTaken | ClosedGroup | DeferredGroup | DormantGroup
  deriving (Eq, Show)

data Group = Group
  { kind :: GroupKind
  , entries :: [Entry]
  }

data Entry = Entry
  { question :: Question
  , detail :: Detail
  }

data Detail
  = Choosing Choices
  | -- | The raisers none of whose options is impossible.
    RaisedIf [NonEmpty OptionId]
  | Took [OptionId]
  | NoDetail

-- | A question's possible options by preference, its implicit other only
-- when at most one listed option is possible, then its taken ones.
data Choices = Choices
  { possible :: [Choice]
  , taken :: [OptionId]
  }

data Choice = Choice
  { option :: OptionId
  , rank :: Maybe Rank
  , bounds :: Maybe Bounds
  }

-- | Returns the question at the point, bounding its options.
questionView :: Monad m => Solver m -> Model -> Text -> Analysis -> Question -> m QuestionView
questionView solver m name a q = do
  bs <- maybe (pure Map.empty) (\s -> bounds solver m name s (choices q)) (standing a)
  pure (questionViewFrom m name a bs q)

questionViewFrom :: Model -> Text -> Analysis -> Map OptionId Bounds -> Question -> QuestionView
questionViewFrom m name a bs q =
  QuestionView
    { point = name
    , question = q
    , analysis = a
    , rows = map (optionRow m name a bs) (byPreference (preferenceAt m.points name) q)
    }

optionRow :: Model -> Text -> Analysis -> Map OptionId Bounds -> OptionId -> OptionRow
optionRow m name a bs = row
  where
    ranks = preferenceAt m.points name
    rulings = effective m.points name
    dominators = dominance m name bs
    row o =
      OptionRow
        { option = o
        , title = titleOf m.space o
        , verdict = verdictAt a o
        , ruling = Map.lookup o rulings
        , bounds = Map.lookup o bs
        , rank = Map.lookup o ranks
        , assessment = assessmentOf m.space o
        , dominated = domination bs o <$> Map.lookup o dominators
        , raises = raisedQuestions m.space (Set.singleton o)
        , objections = case standing a of
            Just s -> objectionsTo m.space s o
            Nothing -> []
        }

-- | An option of a question at a point, with every set of rulings making
-- it forced or impossible.
data OptionView = OptionView
  { point :: Text
  , question :: Question
  , analysis :: Analysis
  , row :: OptionRow
  , description :: Text
  , causes :: [Set Literal]
  -- ^ Empty when the option's own ruling is the only cause.
  }

-- | Returns the option of the question at the point, bounding the
-- question's options.
optionView :: Monad m => Solver m -> Model -> Text -> Analysis -> Question -> OptionId -> m OptionView
optionView solver m name a q o = do
  bs <- maybe (pure Map.empty) (\s -> bounds solver m name s (choices q)) (standing a)
  let row = optionRow m name a bs o
  cs <- maybe (pure []) (causes solver m name o) (mfilter (/= Possible) row.verdict)
  let ownRuling = [Set.singleton l | Just (l, _, _) <- [row.ruling]]
  pure
    OptionView
      { point = name
      , question = q
      , analysis = a
      , row
      , description = case o of
          Listed i -> maybe "" (\(_, opt) -> opt.description) (optionAt m.space i)
          Other _ -> ""
      , causes = if cs == ownRuling then [] else cs
      }

domination :: Map OptionId Bounds -> OptionId -> OptionId -> Domination
domination bs o p = Domination p (bs Map.! o).hi (bs Map.! p).lo

objectionsTo :: Space -> Standing -> OptionId -> [Objection]
objectionsTo space s o =
  [ Objection
      { raising = r
      , state = s.states Map.! r.question.id
      , options = [RaisedOption c (s.verdicts Map.! c) (objectionsTo space s c) | c <- listedThenOther r.question]
      }
  | r <- raisedQuestions space (Set.singleton o)
  ]

-- | Returns the bounds of the options of the point's open questions.
statusBounds :: Monad m => Solver m -> Model -> Text -> Analysis -> m (Map OptionId Bounds)
statusBounds solver m name a = maybe (pure Map.empty) (\s -> bounds solver m name s (openOptions m.space s)) (standing a)

openOptions :: Space -> Standing -> [OptionId]
openOptions space s = [o | q <- Map.elems space.questions, isOpen (s.states Map.! q.id), o <- choices q]

-- | Returns every group, open questions in the order to decide them.
statusGroups :: Model -> Text -> Map OptionId Bounds -> Standing -> [Group]
statusGroups m name bs s =
  [ Group Frontier [Entry q (Choosing (choicesAt m name bs s q)) | (q, Raised) <- frontier]
  , Group TakenMore [Entry q (Choosing (choicesAt m name bs s q)) | (q, Taken) <- frontier]
  , Group RaisedIfTaken [Entry q (RaisedIf (toList rs)) | (q, Unraised rs) <- frontier]
  , Group ClosedGroup [Entry q (Took (taken s.verdicts q)) | q <- inState Closed]
  , Group DeferredGroup [Entry q NoDetail | q <- inState Deferred]
  , Group DormantGroup [Entry q NoDetail | q <- inState Dormant]
  ]
  where
    inState st = [q | q <- Map.elems m.space.questions, s.states Map.! q.id == st]
    frontier =
      [ (q, openness)
      | block <- decisionOrder m.space (Map.keysSet (Map.filter isOpen s.states))
      , i <- block
      , let q = m.space.questions Map.! i
      , Open openness <- [s.states Map.! i]
      ]

choicesAt :: Model -> Text -> Map OptionId Bounds -> Standing -> Question -> Choices
choicesAt m name bs s q =
  Choices
    { possible = [Choice o (Map.lookup o ranks) (Map.lookup o bs) | o <- shown]
    , taken = taken s.verdicts q
    }
  where
    ranks = preferenceAt m.points name
    possibleOptions = [o | o <- byPreference ranks q, Map.lookup o s.verdicts == Just Possible]
    shown = case filter (/= Other q.id) possibleOptions of
      several@(_ : _ : _) -> several
      _ -> possibleOptions

-- | Returns every point, given the stale ones.
pointViews :: Model -> Set Text -> [PointView]
pointViews m staleNames =
  [ PointView
      { point = p
      , stale = Set.member p.name staleNames
      , rulesNothingElse = maybe False (rulesAlike p.name) p.extends
      }
  | p <- Map.elems m.points
  ]
  where
    rulesAlike p e = rulingsOf m.points p == rulingsOf m.points e && deferredAt m.points p == deferredAt m.points e

-- | The model with suppositions applied, what would move, and the
-- questions the supposed acceptances raise.
data Whatif = Whatif
  { model :: Model
  , moved :: Moved
  , implications :: [Implication]
  }

whatifFacts :: Monad m => Solver m -> Model -> Text -> [Supposition] -> m (Either EditError Whatif)
whatifFacts solver m name ss = whatif solver m name ss >>= traverse withImplications
  where
    accepted = Set.fromList [o | Suppose (Take o) <- ss]
    withImplications (m', mv) = Whatif m' mv <$> maybe (pure []) (\s -> implications solver m' name s accepted) (standing mv.after)
