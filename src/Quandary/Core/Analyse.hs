module Quandary.Core.Analyse
  ( QuestionState (..)
  , Analysis (..)
  , Standing (..)
  , Moved (..)
  , Bounds (..)
  , Implication (..)
  , Refusal (..)
  , analyse
  , standing
  , verdictAt
  , onlyOptionsMoved
  , bounds
  , implications
  , topGrade
  , whyAll
  , edit
  , whatif
  , stale
  , staleness
  , stalePoints
  , everyStale
  , Adoption (..)
  , AdoptError (..)
  , adopt
  )
where

import Control.Monad (filterM, foldM, forM)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Compile (compile)
import Quandary.Core.Rulings
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))

data QuestionState = Open | Closed | Deferred | Dormant
  deriving (Eq, Ord, Show)

data Analysis
  = -- | No design reaches the rulings; holds every minimal subset that
    -- none reaches.
    StaleAt [Set Literal]
  | Fresh Standing
  deriving (Eq, Show)

data Standing = Standing
  { verdicts :: Map OptionId Verdict
  , states :: Map Id QuestionState
  }
  deriving (Eq, Show)

analyse :: Monad m => Solver m -> Model -> Text -> m Analysis
analyse (Solver solving) m name = solving f $ \query ->
  consequences query rulings >>= \case
    Stale -> StaleAt <$> conflicts query rulings Set.empty
    Reaches verdicts -> Fresh . Standing verdicts <$> states query verdicts
  where
    f = compile m.space
    rulings = rulingsOf m.points name
    deferred = deferredAt m.points name
    states query verdicts = traverse stateOf m.space.questions
      where
        stateOf q = do
          dormant <- if null q.raisedBy then pure False else allM dead q.raisedBy
          pure (state dormant q)
        state dormant q
          | dormant = Dormant
          | closed q = Closed
          | Set.member q.id deferred = Deferred
          | otherwise = Open
        dead r = not <$> reaches query (Set.union rulings (Set.fromList (map Take (toList r))))
        closed q = case q.pick of
          One -> any (\o -> Map.lookup o verdicts == Just Forced) (choices q)
          _ -> all (\o -> Map.lookup o verdicts /= Just Possible) (choices q)

verdictAt :: Analysis -> OptionId -> Maybe Verdict
verdictAt a opt = standing a >>= Map.lookup opt . (.verdicts)

standing :: Analysis -> Maybe Standing
standing (Fresh s) = Just s
standing (StaleAt _) = Nothing

-- | The grades, as positions in the space's grades, of the best design
-- taking an option: counting each ungraded option at the lowest grade,
-- and at the highest.
data Bounds = Bounds
  { lo :: Int
  , hi :: Int
  }
  deriving (Eq, Show)

-- | Returns the position of the highest grade.
topGrade :: Space -> Int
topGrade space = length space.grades - 1

-- | A variable of the formula bounding an option.
data Var
  = -- | The design takes the option.
    Chosen OptionId
  | -- | Taking the option bounded forces this one, where the point does not.
    Seed OptionId
  | -- | The option is one the option bounded commits to.
    Committed OptionId
  deriving (Eq, Ord, Show)

-- | Returns the bounds of each given option some design reaching the
-- point takes, given the point's analysis. An option commits to itself,
-- to the options taking it forces that the point does not, and to the
-- options a design takes in the questions its commitments raise. A
-- design's grade for the option is the lowest grade among the
-- commitments it takes; an unverified grade counts toward the upper bound
-- only.
bounds :: Monad m => Solver m -> Model -> Text -> Analysis -> [OptionId] -> m (Map OptionId Bounds)
bounds (Solver solving) m name analysis targets = case analysis of
  Fresh s | not (null m.space.grades), not (null targets) -> solving formula (each s)
  _ -> pure Map.empty
  where
    each s query =
      fmap Map.fromList . forM [o | o <- targets, maybe False (/= Impossible) (Map.lookup o s.verdicts)] $ \o -> do
        -- A seed counts only if its commitments can reach an option graded
        -- below the cap.
        let bound (grade, below) cap = case Map.lookup cap below of
              Just relevant | not (Set.null relevant) -> do
                held <- backbone query (Set.insert (Take (Chosen o)) chosenRulings) (Set.map (Take . Chosen) (Set.delete o (Set.difference relevant forced)))
                let seeds = foldMap (\ls -> Set.fromList [p | Take (Chosen p) <- Set.toList ls]) held
                best query o (Set.insert o seeds) grade cap
              _ -> pure cap
        hi <- bound highs top
        lo <- bound lows (min hi (low o))
        pure (o, Bounds lo hi)
      where
        forced = Map.keysSet (Map.filter (== Forced) s.verdicts)
    top = topGrade m.space
    rank = Map.fromList (zip m.space.grades [0 ..])
    at = optionAt m.space
    assessed (Listed i) = at i >>= (\(_, o) -> o.assessment)
    assessed (Other _) = Nothing
    gradeOf a = Map.findWithDefault 0 a.grade rank
    low o = maybe 0 (\a -> if a.unverified then 0 else gradeOf a) (assessed o)
    high o = maybe top gradeOf (assessed o)
    -- For each grade above the lowest, the options graded below it and the
    -- options whose commitments can reach one.
    reachingBelow grade = Map.fromList [(k, closure (Set.fromList [p | p <- options, grade p < k])) | k <- [1 .. top]]
    closure r =
      let r' = Set.union r (Set.fromList [c | q <- Map.elems m.space.questions, any (`Set.member` r) (choices q), g <- q.raisedBy, c <- toList g])
       in if r' == r then r else closure r'
    lows = (low, reachingBelow low)
    highs = (high, reachingBelow high)
    f = compile m.space
    options = Set.toList f.variables
    chosenRulings = Set.map (fmap Chosen) (rulingsOf m.points name)
    formula =
      Formula
        { variables = Set.fromList (concat [[Chosen o, Seed o, Committed o] | o <- options])
        , forbidden =
            Set.unions
              [ Set.map (Set.map (fmap Chosen)) f.forbidden
              , Set.fromList [Set.fromList [Take (Seed o), Refuse (Committed o)] | o <- options]
              , Set.fromList
                  [ Set.fromList ([Take (Chosen o), Take (Committed c), Refuse (Committed o)] ++ [Take (Chosen x) | x <- toList g])
                  | q <- Map.elems m.space.questions
                  , g <- q.raisedBy
                  , c <- toList g
                  , o <- choices q
                  ]
              ]
        }
    -- Start the search at the cap or the lowest seed's grade.
    best query o seeds grade cap = go (minimum (cap : map grade (Set.toList seeds)))
      where
        go 0 = pure 0
        go k = do
          r <- query.design (Set.unions [chosenRulings, Set.fromList (Take (Chosen o) : map (Take . Seed) (Set.toList seeds)), Set.fromList [Refuse (Committed p) | p <- options, grade p < k]])
          maybe (go (k - 1)) (const (pure k)) r

-- | A question some options raise, with the bounds of its possible
-- options, or none when it is dormant.
data Implication = Implication
  { raising :: Raising
  , values :: Maybe (Map OptionId Bounds)
  }
  deriving (Eq, Show)

-- | Returns each question raised by the given options the point accepts,
-- with the bounds of its possible options at the point.
implications :: Monad m => Solver m -> Model -> Text -> Standing -> Set OptionId -> m [Implication]
implications solver m name s os = do
  bs <- bounds solver m name (Fresh s) (concatMap (possible . (.question)) (filter (not . dormant) rs))
  pure [Implication r (valuesAt bs r) | r <- rs]
  where
    rs = raisedQuestions m.space (Set.filter (\o -> Set.member (Take o) (rulingsOf m.points name)) os)
    dormant r = Map.lookup r.question.id s.states == Just Dormant
    possible q = [o | o <- choices q, Map.lookup o s.verdicts == Just Possible]
    valuesAt bs r
      | dormant r = Nothing
      | otherwise = Just (Map.restrictKeys bs (Set.fromList (choices r.question)))

-- | Returns every minimal subset of the rulings that leaves no design
-- holding the literal, smallest first.
whyAll :: Monad m => Solver m -> Space -> Set Literal -> Literal -> m [Set Literal]
whyAll (Solver solving) space rulings l = solving (compile space) (\query -> conflicts query rulings (Set.singleton l))

-- | Returns every minimal subset of the rulings that no valid design
-- holding the given literals reaches, smallest first.
conflicts :: Monad m => Query OptionId m -> Set Literal -> Set Literal -> m [Set Literal]
conflicts query rulings ls = sortOn Set.size <$> go []
  where
    -- Explain under each hitting set in turn, stopping at the first core.
    go found = foldr (\h next -> core query (Set.difference rulings h) ls >>= maybe next (go . (: found))) (pure found) (hittingSets found)

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM p = foldr (\x rest -> p x >>= \ok -> if ok then rest else pure False) (pure True)

-- | Returns the minimal sets that meet every given set.
hittingSets :: Ord a => [Set a] -> [Set a]
hittingSets = foldr extend [Set.empty]
  where
    extend s hs = minimal (Set.toList (Set.fromList [h' | h <- hs, h' <- if Set.disjoint h s then [Set.insert x h | x <- Set.toList s] else [h]]))
    minimal hs = [h | h <- hs, not (any (`Set.isProperSubsetOf` h) hs)]

data Moved = Moved
  { options :: [(OptionId, Verdict, Verdict)]
  -- ^ Each option whose verdict changed, before and after.
  , questions :: [(Id, QuestionState, QuestionState)]
  , before :: Analysis
  , after :: Analysis
  , branches :: [(Text, Maybe [Set Literal])]
  -- ^ Each other point whose staleness changed, and whether it is stale now.
  }
  deriving (Eq, Show)

-- | Holds when no question, branch or staleness moved.
onlyOptionsMoved :: Moved -> Bool
onlyOptionsMoved mv = null mv.questions && null mv.branches && stale mv.after == stale mv.before

-- | Returns what moves at the named point and the points extending it
-- between the two models.
movement :: Monad m => Solver m -> Model -> Model -> Text -> m Moved
movement solver m m' name = moved <$> analyse solver m name <*> analyse solver m' name <*> extending solver m m' name

moved :: Analysis -> Analysis -> [(Text, Maybe [Set Literal])] -> Moved
moved before after branches =
  Moved
    { options = changes (.verdicts)
    , questions = changes (.states)
    , before
    , after
    , branches
    }
  where
    changes :: (Ord k, Eq v) => (Standing -> Map k v) -> [(k, v, v)]
    changes field = case (before, after) of
      (Fresh b, Fresh a) ->
        [ (k, x, y)
        | (k, (x, y)) <- Map.toList (Map.intersectionWith (,) (field b) (field a))
        , x /= y
        ]
      _ -> []

-- | Returns each minimal set of the point's rulings no design reaches, or
-- Nothing when the point is fresh.
staleness :: Monad m => Solver m -> Model -> Text -> m (Maybe [Set Literal])
staleness (Solver solving) m name = solving (compile m.space) $ \query ->
  reaches query rulings >>= \case
    True -> pure Nothing
    False -> Just <$> conflicts query rulings Set.empty
  where
    rulings = rulingsOf m.points name

-- | Returns the named points no design reaches.
stalePoints :: Monad m => Solver m -> Model -> [Text] -> m (Set Text)
stalePoints (Solver solving) m names = solving (compile m.space) $ \query ->
  Set.fromList <$> filterM (fmap not . reaches query . rulingsOf m.points) names

-- | Returns the stale points among all the model's points.
everyStale :: Monad m => Solver m -> Model -> m (Set Text)
everyStale solver m = stalePoints solver m (Map.keys m.points)

stale :: Analysis -> Bool
stale (StaleAt _) = True
stale (Fresh _) = False

data Refusal
  = Refused EditError
  | -- | The ruling would leave the point stale; holds each minimal set of
    -- rulings, the new one included, that no design reaches.
    WouldBeStale [Set Literal]
  deriving (Eq, Show)

-- | Apply the edits in order to the named point, returning the model as
-- changed and what moved. Refuses an accept or defeat that leaves the
-- point stale.
edit :: Monad m => Solver m -> Model -> Text -> [Edit] -> m (Either Refusal (Model, Moved))
edit solver m name es =
  whatif solver m name es >>= \case
    Left e -> pure (Left (Refused e))
    Right (_, mv) | StaleAt why <- mv.after, any (isJust . ruledLiteral) es -> pure (Left (WouldBeStale why))
    Right r -> pure (Right r)

-- | Returns each point extending the named one whose staleness differs
-- between the two models, with the sets of rulings no design reaches
-- when it is stale in the second.
extending :: Monad m => Solver m -> Model -> Model -> Text -> m [(Text, Maybe [Set Literal])]
extending solver m m' name = do
  was <- stalePoints solver m ns
  now <- stalePoints solver m' ns
  forM [n | n <- ns, Set.member n was /= Set.member n now] $ \n -> (,) n <$> staleness solver m' n
  where
    ns = descendants m.points name

-- | Returns the model with the edits applied in order, and what would
-- move.
whatif :: Monad m => Solver m -> Model -> Text -> [Edit] -> m (Either EditError (Model, Moved))
whatif solver m name es = case applyEdits m name es of
  Left e -> pure (Left e)
  Right m' -> Right . (,) m' <$> movement solver m m' name

applyEdits :: Model -> Text -> [Edit] -> Either EditError Model
applyEdits m name es = (\points -> m {points}) <$> foldM (\ps e -> (\p -> Map.insert name p ps) <$> applyEdit m.space ps name e) m.points es

data Adoption = Adoption
  { model :: Model
  -- ^ The model with every point as changed, the branch gone.
  , into :: Text
  -- ^ The point adopting.
  , moved :: Moved
  -- ^ What moved at the point adopting.
  , dropped :: [(Text, [OptionId])]
  -- ^ Each point that dropped inherited rulings to stay fresh, and what it
  -- dropped.
  }
  deriving (Eq, Show)

data AdoptError
  = UnknownBranch
  | ExtendsNothing
  | -- | Holds each minimal set of the branch's rulings no design reaches.
    StaleBranch [Set Literal]
  deriving (Eq, Show)

-- | Move a branch's own rulings into the point it extends and remove the
-- branch. Points extending the branch extend that point instead. Any
-- other point extending that point which the adoption leaves stale drops
-- the fewest adopted rulings that clear its conflicts, and loses the
-- rulings it now inherits unchanged.
adopt :: Monad m => Solver m -> Model -> Text -> m (Either AdoptError Adoption)
adopt solver m name = case Map.lookup name m.points of
  Nothing -> pure (Left UnknownBranch)
  Just b -> case b.extends of
    Nothing -> pure (Left ExtendsNothing)
    Just parent ->
      analyse solver m name >>= \case
        StaleAt cores -> pure (Left (StaleBranch cores))
        Fresh _ -> Right <$> adoptInto b parent
  where
    adoptInto b parent = do
      let p = pointNamed m.points parent
          inheritedAtParent = Map.keysSet (effective m.points parent)
          ownAtParent o = Map.member o p.accept || Map.member o p.defeat
          p' =
            p
              { accept = Map.union b.accept (Map.withoutKeys p.accept (Set.union (Map.keysSet b.defeat) b.drop))
              , defeat = Map.union b.defeat (Map.withoutKeys p.defeat (Set.union (Map.keysSet b.accept) b.drop))
              , drop =
                  Set.union
                    (Set.difference p.drop (Set.union (Map.keysSet b.accept) (Map.keysSet b.defeat)))
                    (Set.filter (\o -> not (ownAtParent o) && Set.member o inheritedAtParent) b.drop)
              , defer = Set.union p.defer b.defer
              , prefer = Map.union b.prefer p.prefer
              }
          rebased =
            Map.map (\x -> if x.extends == Just name then x {extends = Just parent} else x) $
              Map.insert parent p' (Map.delete name m.points)
          adopted = Set.unions [Map.keysSet b.accept, Map.keysSet b.defeat, b.drop]
          m' = m {points = rebased}
      (fixed, dropped) <- foldM (settle adopted) (m', []) (reverse (descendants rebased parent))
      mv <- movement solver m {points = Map.delete name m.points} fixed parent
      pure Adoption {model = fixed {points = Map.map (\x -> if x.extends == Just parent then prune fixed.points x else x) fixed.points}, into = parent, moved = mv, dropped}
    prune ps x = case x.extends of
      Nothing -> x
      Just e ->
        let inherited = effective ps e
            same l o r = case Map.lookup o inherited of
              Just (l', _, r') -> l' == l o && r' == r
              Nothing -> False
         in x {accept = Map.filterWithKey (\o r -> not (same Take o r)) x.accept, defeat = Map.filterWithKey (\o r -> not (same Refuse o r)) x.defeat}
    settle adopted (now, ds) n =
      analyse solver now n >>= \case
        Fresh _ -> pure (now, ds)
        StaleAt cores ->
          let x = pointNamed now.points n
              droppable l = Set.member (optionOf l) adopted && inheritedAt now.points n l
              hits = hittingSets [Set.map optionOf (Set.filter droppable c) | c <- cores]
           in pure $ case sortOn Set.size hits of
                h : _
                  | not (any (Set.null . Set.filter droppable) cores) ->
                      (now {points = Map.insert n x {drop = Set.union x.drop h} now.points}, (n, Set.toList h) : ds)
                _ -> (now, ds)
