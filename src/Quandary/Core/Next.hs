-- | Which options to grade to move an option's bounds.
module Quandary.Core.Next
  ( Lead (..)
  , Move (..)
  , leads
  , leadsOf
  )
where

import Data.List (minimumBy, partition, sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Analyse (Bounds (..), Standing (..), Var (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings qualified as Rulings
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))

-- | The fewest options whose grades move an option's bounds.
data Lead = Lead
  { option :: OptionId
  , bounds :: Bounds
  , move :: Move
  , grading :: Set OptionId
  }
  deriving (Eq, Show)

data Move
  = -- | Each graded at least this grade raises `lo`.
    Raise Int
  | -- | Each graded below this grade lowers `hi`.
    Lower Int
  deriving (Eq, Show)

-- | What one solver session over the bounding formula needs at a point.
data Session m = Session
  { query :: Query Var m
  , bound :: OptionId -> m (Maybe Bounds)
  , space :: Space
  , rulings :: Set (Lit Var)
  , unforced :: Set OptionId
  , options :: [OptionId]
  }

-- | Returns a lead for each of the first given number of possible options
-- of root questions, most preferred first, whose bounds grading can move:
-- the cheaper of raising `lo` and lowering `hi`, raising when they cost
-- the same.
leads :: Monad m => Solver m -> Model -> Text -> Standing -> Int -> m [Lead]
leads solver m name s n = inSession solver m name s (\se -> firstLeads se n (rootOptions m name s))

-- | Returns the lead of each option whose bounds grading can move, from the
-- bounds given.
leadsOf :: Monad m => Solver m -> Model -> Text -> Standing -> [(OptionId, Bounds)] -> m [Lead]
leadsOf solver m name s obs = case filter (\(_, b) -> b.lo < b.hi) obs of
  [] -> pure []
  open -> inSession solver m name s (\se -> catMaybes <$> mapM (uncurry (leadFor se)) open)

-- | Run the continuation in one session at the point, or return nothing
-- when the space declares no grades.
inSession :: (Monad m, Monoid a) => Solver m -> Model -> Text -> Standing -> (Session m -> m a) -> m a
inSession Solver {solving} m name s k
  | null m.space.grades = pure mempty
  | otherwise = solving formula (k . session)
  where
    (formula, each) = Analyse.bounding m name s
    session query =
      Session
        { query
        , bound = \o -> Map.lookup o <$> each query [o]
        , space = m.space
        , rulings = Analyse.chosenRulings m name
        , unforced = Map.keysSet (Map.filter (/= Forced) s.verdicts)
        , options = [o | Chosen o <- Set.toList query.variables]
        }

-- | Returns the possible listed options of root questions, most preferred
-- first, then by question and option.
rootOptions :: Model -> Text -> Standing -> [OptionId]
rootOptions m name s =
  sortOn
    (Down . (`Map.lookup` ranks))
    [ o
    | q <- Map.elems m.space.questions
    , null q.raisedBy
    , o@(Listed _) <- choices q
    , Map.lookup o s.verdicts == Just Possible
    ]
  where
    ranks = Rulings.preferenceAt m.points name

firstLeads :: Monad m => Session m -> Int -> [OptionId] -> m [Lead]
firstLeads _ 0 _ = pure []
firstLeads _ _ [] = pure []
firstLeads se k (o : os) = leadAt se o >>= maybe (firstLeads se k os) (\l -> (l :) <$> firstLeads se (k - 1) os)

-- | Returns the option's lead, or Nothing when its bounds meet or no
-- grading moves them.
leadAt :: Monad m => Session m -> OptionId -> m (Maybe Lead)
leadAt se o =
  se.bound o >>= \case
    Just b | b.lo < b.hi -> leadFor se o b
    _ -> pure Nothing

leadFor :: Monad m => Session m -> OptionId -> Bounds -> m (Maybe Lead)
leadFor se o b = do
  seeds <- seedsOf se o
  let held = Analyse.heldFor se.rulings o seeds
  raising <- fmap (lead (Raise (b.lo + 1))) <$> raisingSet se held (b.lo + 1)
  lowering <- fmap (lead (Lower b.hi)) <$> loweringSet se held seeds b.hi
  pure (cheapest (catMaybes [raising, lowering]))
  where
    lead move grading = Lead {option = o, bounds = b, move, grading}

-- | Returns the lead grading fewest options, the first among equals.
cheapest :: [Lead] -> Maybe Lead
cheapest = fmap (minimumBy (comparing (Set.size . (.grading)))) . nonEmpty

-- | Returns the option and the options every design taking it takes that
-- the point does not force.
seedsOf :: Monad m => Session m -> OptionId -> m (Set OptionId)
seedsOf se o =
  Set.insert o . foldMap Analyse.takenOptions
    <$> backbone se.query (Set.insert (Take (Chosen o)) se.rulings) (Set.map (Take . Chosen) se.unforced)

-- | Returns the fewest options without a verified grade that, graded at
-- least the grade given, give a design holding the literals whose every
-- commitment is graded at least that; Nothing when no grading does.
raisingSet :: Monad m => Session m -> Set (Lit Var) -> Int -> m (Maybe (Set OptionId))
raisingSet se held g = reaches se.query fixed >>= \r -> if r then Just <$> go [] else pure Nothing
  where
    low = Analyse.lowGrade se.space
    assessed = Rulings.assessmentOf se.space
    gradable o@(Listed _) = maybe True (.unverified) (assessed o)
    gradable (Other _) = False
    (candidates, blocked) = partition gradable [o | o <- se.options, low o < g]
    fixed = Set.union held (Set.fromList (map (Refuse . Committed) blocked))
    go cores = do
      let h = Analyse.smallestHittingSet cores
      core se.query (Set.fromList [Refuse (Committed o) | o <- candidates, Set.notMember o h]) fixed >>= \case
        Nothing -> pure h
        Just c -> go (Set.fromList [o | Refuse (Committed o) <- Set.toList c] : cores)

-- | Returns the fewest ungraded options that, graded below the grade given,
-- leave no design holding the literals whose every commitment, from the
-- seeds given, is graded at least that; Nothing when no grading does.
loweringSet :: Monad m => Session m -> Set (Lit Var) -> Set OptionId -> Int -> m (Maybe (Set OptionId))
loweringSet se held seeds h = go []
  where
    high = Analyse.highGrade se.space
    assessed = Rulings.assessmentOf se.space
    candidates = Set.fromList [o | o@(Listed _) <- se.options, isNothing (assessed o)]
    fixed = Set.union held (Set.fromList [Refuse (Committed o) | o <- se.options, high o < h])
    within = design se.query fixed
    chosen = Set.fromList (map Chosen se.options)
    lowerable d = Set.intersection candidates (commitments se.space seeds (Set.fromList [o | Chosen o <- Set.toList d]))
    go sets = do
      let hit = Analyse.smallestHittingSet sets
      within (Set.map (Refuse . Committed) hit) chosen >>= \case
        Nothing -> pure (Just hit)
        Just d
          | Set.null (lowerable d) -> pure Nothing
          | otherwise -> go (lowerable d : sets)

-- | Returns what a design taking the options given commits to: the seeds,
-- and each option it takes in a question raised by a group it takes
-- holding a commitment.
commitments :: Space -> Set OptionId -> Set OptionId -> Set OptionId
commitments space seeds chosen = grow seeds
  where
    grow c =
      let c' = Set.union c (raised c)
       in if c' == c then c else grow c'
    raised c =
      Set.fromList
        [ o
        | q <- Map.elems space.questions
        , any (holdsCommitment c) q.raisedBy
        , o <- choices q
        , Set.member o chosen
        ]
    holdsCommitment c g = all (`Set.member` chosen) g && any (`Set.member` c) g
