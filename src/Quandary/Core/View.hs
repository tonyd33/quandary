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
  , Dead (..)
  , hidesQuestion
  , hidesOption
  , BoxGroup (..)
  , BoxRow (..)
  , Cell (..)
  , questionView
  , questionViewFrom
  , optionView
  , optionDescription
  , statusBounds
  , statusGroups
  , groupKind
  , choicesAt
  , boxGroups
  , pointViews
  , whatifFacts
  )
where

import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
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
  , rank :: Maybe Rank
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
    , rows = map (optionRow m name ranks a bs) (byPreference ranks q)
    }
  where
    ranks = preferenceAt m.points name

optionRow :: Model -> Text -> Map OptionId Rank -> Analysis -> Map OptionId Bounds -> OptionId -> OptionRow
optionRow m name ranks a bs = row
  where
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
            Just s -> objectionsTo m.space ranks s o
            Nothing -> []
        }

-- | An option of a question at a point, with every set of rulings making
-- it forced or impossible.
data OptionView = OptionView
  { point :: Text
  , question :: Question
  , analysis :: Analysis
  , row :: OptionRow
  , causes :: [Cause]
  -- ^ Empty when the option's own ruling is the only cause.
  }

-- | Returns the option's view at the point, with bounds over its
-- question's options unless it is impossible.
optionView :: Monad m => Solver m -> Model -> Text -> Analysis -> Question -> OptionId -> m OptionView
optionView solver m name a q o = do
  (bs, cs) <- boundsAndCausesAt solver m name a q o
  let row = optionRow m name (preferenceAt m.points name) a bs o
  explained <- explainAt solver m row (beyondOwnRuling row cs)
  pure
    OptionView
      { point = name
      , question = q
      , analysis = a
      , row
      , causes = explained
      }

-- | Returns each set of rulings giving the row's option its verdict, with
-- the constraints under which they give it.
explainAt :: Monad m => Solver m -> Model -> OptionRow -> [Set Literal] -> m [Cause]
explainAt _ _ _ [] = pure []
explainAt solver m row cs = case row.verdict of
  Just v -> explain solver m.space row.option v cs
  Nothing -> pure []

-- | Returns the bounds of the question's options when the option is
-- possible or forced, and every set of rulings making it forced or
-- impossible.
boundsAndCausesAt ::
  Monad m => Solver m -> Model -> Text -> Analysis -> Question -> OptionId -> m (Map OptionId Bounds, [Set Literal])
boundsAndCausesAt solver m name a q o = case (standing a, verdictAt a o) of
  (Just s, Just Possible) -> (\bs -> (bs, [])) <$> bounds solver m name s (choices q)
  (Just s, Just Forced) -> boundsAndCauses solver m name s (choices q) o Forced
  (_, Just Impossible) -> (,) Map.empty <$> causes solver m name o Impossible
  _ -> pure (Map.empty, [])

-- | Returns the sets of rulings, none when the option's own ruling is the
-- only one.
beyondOwnRuling :: OptionRow -> [Set Literal] -> [Set Literal]
beyondOwnRuling row cs
  | cs == ownRuling = []
  | otherwise = cs
  where
    ownRuling = [Set.singleton l | (l, _, _) <- maybeToList row.ruling]

-- | Returns the option's description, empty for an implicit other.
optionDescription :: OptionView -> Text
optionDescription v = maybe "" (.description) (find (\o -> Listed o.id == v.row.option) v.question.options)

domination :: Map OptionId Bounds -> OptionId -> OptionId -> Domination
domination bs o p = Domination p (bs Map.! o).hi (bs Map.! p).lo

objectionsTo :: Space -> Map OptionId Rank -> Standing -> OptionId -> [Objection]
objectionsTo space ranks s o =
  [ Objection
      { raising = r
      , state = s.states Map.! r.question.id
      , options =
          [ RaisedOption c (s.verdicts Map.! c) (Map.lookup c ranks) (objectionsTo space ranks s c)
          | c <- listedThenOther r.question
          ]
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

-- | Returns the status group a question in the state is listed under.
groupKind :: QuestionState -> GroupKind
groupKind (Open Raised) = Frontier
groupKind (Open Taken) = TakenMore
groupKind (Open (Unraised _)) = RaisedIfTaken
groupKind Closed = ClosedGroup
groupKind Deferred = DeferredGroup
groupKind Dormant = DormantGroup

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

-- | Whether a view leaves out dormant questions and impossible options.
data Dead = HideDead | ShowDead
  deriving (Eq, Ord, Show)

-- | Holds when the view leaves out a question in the state.
hidesQuestion :: Dead -> Maybe QuestionState -> Bool
hidesQuestion HideDead (Just Dormant) = True
hidesQuestion _ _ = False

-- | Holds when the view leaves out an option with the verdict.
hidesOption :: Dead -> Maybe Verdict -> Bool
hidesOption HideDead (Just Impossible) = True
hidesOption _ _ = False

-- | A status group with every option of each of its questions.
data BoxGroup = BoxGroup
  { kind :: GroupKind
  , rows :: [BoxRow]
  }

data BoxRow = BoxRow
  { question :: Question
  , was :: Maybe QuestionState
  -- ^ Its state before, when it differs.
  , cells :: [Cell]
  -- ^ By preference at the point.
  }

data Cell = Cell
  { option :: OptionId
  , title :: Maybe Text
  , verdict :: Verdict
  , was :: Maybe Verdict
  -- ^ Its verdict before, when it differs.
  , rank :: Maybe Rank
  , ruling :: Maybe (Literal, Text, Ruling)
  }

-- | Returns every status group holding a question the view shows, each
-- question with the options it shows. Given the standing before, each
-- question and option carries what it was when that differs, and is shown
-- when it was shown before.
boxGroups :: Model -> Text -> Dead -> Maybe Standing -> Standing -> [BoxGroup]
boxGroups m name dead before s =
  [ BoxGroup g.kind rows
  | g <- statusGroups m name Map.empty s
  , let rows = [row e.question | e <- g.entries, shown (hidesQuestion dead) (stateOf e.question.id)]
  , not (null rows)
  ]
  where
    ranks = preferenceAt m.points name
    rulings = effective m.points name
    stateOf i st = Map.lookup i st.states
    verdictOf o st = Map.lookup o st.verdicts
    shown :: (Maybe a -> Bool) -> (Standing -> Maybe a) -> Bool
    shown hides at = not (hides (at s) && maybe True (hides . at) before)
    row q =
      BoxRow
        q
        (changed (stateOf q.id))
        [cell o | o <- byPreference ranks q, shown (hidesOption dead) (verdictOf o)]
    cell o =
      Cell
        { option = o
        , title = titleOf m.space o
        , verdict = s.verdicts Map.! o
        , was = changed (verdictOf o)
        , rank = Map.lookup o ranks
        , ruling = Map.lookup o rulings
        }
    changed :: Eq a => (Standing -> Maybe a) -> Maybe a
    changed at = case before of
      Just b | at b /= at s -> at b
      _ -> Nothing

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
