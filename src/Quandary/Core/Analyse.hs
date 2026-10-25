module Quandary.Core.Analyse
  ( QuestionState (..)
  , Openness (..)
  , Analysis (..)
  , Standing (..)
  , Moved (..)
  , Bounds (..)
  , Implication (..)
  , Refusal (..)
  , analyse
  , standing
  , verdictAt
  , taken
  , bounds
  , boundsAndCauses
  , dominance
  , implications
  , topGrade
  , whyAll
  , causes
  , Cause (..)
  , explain
  , edit
  , whatif
  , Supposition (..)
  , Step (..)
  , Remark (..)
  , stale
  , isOpen
  , changedQuestions
  , staleness
  , stalePoints
  , everyStale
  , Adoption (..)
  , AdoptError (..)
  , Confirmation (..)
  , adopt
  )
where

import Control.Monad (filterM, foldM, forM)
import Data.Foldable (toList)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Quandary.Core.Compile (Constraint, compile)
import Quandary.Core.Compile qualified as Compile
import Quandary.Core.Rulings
import Quandary.Core.Solver
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))

data QuestionState = Open Openness | Closed | Deferred | Dormant
  deriving (Eq, Ord, Show)

isOpen :: QuestionState -> Bool
isOpen (Open _) = True
isOpen _ = False

-- | Where an open question stands, the first that holds.
data Openness
  = -- | Some option is forced.
    Taken
  | -- | A root, or a raiser has every option forced.
    Raised
  | -- | Holds each raiser none of whose options is impossible.
    Unraised (NonEmpty (NonEmpty OptionId))
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
          | otherwise = Open (openness verdicts q)
        dead r = isNothing <$> underRulings (Set.fromList (map Take (toList r))) Set.empty
        underRulings = query.design rulings
        closed q = case q.pick of
          One -> not (null (taken verdicts q))
          _ -> all (\o -> Map.lookup o verdicts /= Just Possible) (choices q)

-- | Preconditions: the question is open and not dormant.
openness :: Map OptionId Verdict -> Question -> Openness
openness verdicts q
  | not (null (taken verdicts q)) = Taken
  | null q.raisedBy || any (all forced) q.raisedBy = Raised
  | otherwise = Unraised (NonEmpty.fromList [g | g <- q.raisedBy, all (\o -> Map.lookup o verdicts /= Just Impossible) g])
  where
    forced o = Map.lookup o verdicts == Just Forced

-- | Returns the question's options every design reaching the point takes.
taken :: Map OptionId Verdict -> Question -> [OptionId]
taken verdicts q = [o | o <- choices q, Map.lookup o verdicts == Just Forced]

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
-- point takes, given the point's standing. An option commits to itself,
-- to the options taking it forces that the point does not, and to the
-- options a design takes in the questions its commitments raise. A
-- design's grade for the option is the lowest grade among the
-- commitments it takes; an unverified grade counts toward the upper bound
-- only.
bounds :: Monad m => Solver m -> Model -> Text -> Standing -> [OptionId] -> m (Map OptionId Bounds)
bounds (Solver solving) m name s targets
  | null m.space.grades || null targets = pure Map.empty
  | otherwise = solving formula each
  where
    (formula, each) = bounding m name s targets

-- | Returns the bounds `bounds` returns and the sets of rulings `causes`
-- returns for the option and verdict, from one solver session.
boundsAndCauses ::
  Monad m =>
  Solver m -> Model -> Text -> Standing -> [OptionId] -> OptionId -> Verdict -> m (Map OptionId Bounds, [Set Literal])
boundsAndCauses solver@(Solver solving) m name s targets o v
  | null m.space.grades || null targets = (,) Map.empty <$> causes solver m name o v
  | otherwise = solving formula (\query -> (,) <$> each query <*> causesIn (chosen options query) m name o v)
  where
    (formula, each) = bounding m name s targets
    options = (compile m.space).variables

-- | Returns the formula bounding options and the queries bounding the
-- given ones.
bounding :: Monad m => Model -> Text -> Standing -> [OptionId] -> (Formula Var, Query Var m -> m (Map OptionId Bounds))
bounding m name s targets = (formula, each)
  where
    each query =
      fmap Map.fromList . forM [o | o <- targets, maybe False (/= Impossible) (Map.lookup o s.verdicts)] $ \o -> do
        -- A seed counts only if its commitments can reach an option graded
        -- below the cap.
        let bound (grade, below) cap = case Map.lookup cap below of
              Just relevant | not (Set.null relevant) -> do
                held <-
                  backbone
                    query
                    (Set.insert (Take (Chosen o)) chosenRulings)
                    (Set.map (Take . Chosen) (Set.delete o (Set.difference relevant forced)))
                let seeds = foldMap (\ls -> Set.fromList [p | Take (Chosen p) <- Set.toList ls]) held
                best query o (Set.insert o seeds) grade cap
              _ -> pure cap
        hi <- bound highs top
        lo <- bound lows (min hi (low o))
        pure (o, Bounds lo hi)
      where
        forced = Map.keysSet (Map.filter (== Forced) s.verdicts)
    top = topGrade m.space
    positions = Map.fromList (zip m.space.grades [0 ..])
    at = optionAt m.space
    assessed (Listed i) = at i >>= (\(_, o) -> o.assessment)
    assessed (Other _) = Nothing
    gradeOf a = Map.findWithDefault 0 a.grade positions
    low o = maybe 0 (\a -> if a.unverified then 0 else gradeOf a) (assessed o)
    high o = maybe top gradeOf (assessed o)
    -- For each grade above the lowest, the options graded below it and the
    -- options whose commitments can reach one.
    reachingBelow grade = Map.fromList [(k, closure (Set.fromList [p | p <- options, grade p < k])) | k <- [1 .. top]]
    closure r =
      let r' =
            Set.union
              r
              (Set.fromList [c | q <- Map.elems m.space.questions, any (`Set.member` r) (choices q), g <- q.raisedBy, c <- toList g])
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
        seeded = query.design (Set.union chosenRulings (Set.fromList (Take (Chosen o) : map (Take . Seed) (Set.toList seeds))))
        go 0 = pure 0
        go k = do
          r <- seeded (Set.fromList [Refuse (Committed p) | p <- options, grade p < k]) Set.empty
          maybe (go (k - 1)) (const (pure k)) r

-- | Returns each option whose upper bound is below a sibling's lower
-- bound, with the sibling of highest lower bound, the most preferred among
-- equals.
dominance :: Model -> Text -> Map OptionId Bounds -> Map OptionId OptionId
dominance m name bs = Map.fromList (concatMap dominated (Map.elems m.space.questions))
  where
    ranks = preferenceAt m.points name
    dominated q = [(o, p) | (o, b) <- graded, Just (p, best) <- [find ((/= o) . fst) leaders], b.hi < best.lo]
      where
        graded = mapMaybe (\o -> (,) o <$> Map.lookup o bs) (byPreference ranks q)
        leaders = sortOn (Down . (.lo) . snd) graded

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
  bs <- bounds solver m name s (concatMap (possible . (.question)) (filter (not . dormant) rs))
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
    go found =
      foldr
        (\h next -> core query (Set.difference rulings h) ls >>= maybe next (go . (: found)))
        (pure found)
        (hittingSets found)

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM p = foldr (\x rest -> p x >>= \ok -> if ok then rest else pure False) (pure True)

-- | Returns the minimal sets that meet every given set.
hittingSets :: Ord a => [Set a] -> [Set a]
hittingSets = foldr extend [Set.empty]
  where
    extend s hs =
      minimal
        (Set.toList (Set.fromList [h' | h <- hs, h' <- if Set.disjoint h s then [Set.insert x h | x <- Set.toList s] else [h]]))
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

-- | Returns the questions whose state moved, leaving out a move from open
-- to open.
changedQuestions :: Moved -> [(Id, QuestionState, QuestionState)]
changedQuestions mv = [(q, x, y) | (q, x, y) <- mv.questions, not (isOpen x && isOpen y)]

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

-- | One edit as applied to the named point.
data Step = Step
  { edit :: Edit
  , applied :: [PointEdit]
  -- ^ A settle's defeats, or the edit itself.
  , before :: Model
  -- ^ The model the edit applied to.
  , model :: Model
  -- ^ The model after the edit.
  , moved :: Moved
  , stated :: Set OptionId
  -- ^ The moved options the edit accounts for: each it rules that took
  -- its ruled verdict, and the implicit other of a question it rules in
  -- or moves the state of, gone impossible.
  , remarks :: [Remark]
  }
  deriving (Eq, Show)

-- | A fact about an applied point edit beyond what moved.
data Remark
  = -- | The rulings before the edit already gave the option the verdict
    -- ruled, with each set of them responsible.
    AlreadyDecided OptionId Verdict [Set Literal]
  | -- | A reopened option keeps its verdict, with each set of rulings
    -- responsible.
    StillDecided OptionId Verdict [Set Literal]
  | -- | The point ruled the option the same way before; the old reason,
    -- then the new.
    ReasonChanged (Maybe Reason) (Maybe Reason)
  | -- | The point inherits the same ruling from the named point.
    AlsoRuledIn Text
  | -- | Accepting forces a raiser of the option's question that nothing
    -- rules.
    CommitsToRaiser OptionId
  | -- | A ranked option no design reaching the point takes.
    ImpossibleHere OptionId
  deriving (Eq, Show)

-- | Apply the edits in order to the named point, returning a step for
-- each. Refuses an accept or defeat that leaves the point stale; the
-- refusal holds the model the refused edit applies to.
edit :: Monad m => Solver m -> Model -> Text -> [Edit] -> m (Either (Model, Refusal) [Step])
edit solver m0 name es = do
  before <- analyse solver m0 name
  was <- stalePoints solver m0 ns
  go m0 before was [] es
  where
    ns = descendants m0.points name
    go _ _ _ done [] = pure (Right (reverse done))
    go m before was done (e : rest) = case expandedOnto m before e of
      Left err -> pure (Left (m, Refused err))
      Right (pes, m') -> do
        after <- analyse solver m' name
        now <- stalePoints solver m' ns
        branches <- restaled solver m' ns was now
        let mv = moved before after branches
        case after of
          StaleAt why | any (isJust . ruledLiteral) pes -> pure (Left (m, WouldBeStale why))
          _ -> do
            rs <- concat <$> traverse (remarksOn solver (Change m m' name mv)) pes
            go m' after now (Step e pes m m' mv (statedBy m.space pes mv) rs : done) rest
    expandedOnto m before e = do
      pes <- expanded before e
      m' <- foldM applyHere m pes
      pure (pes, m')
    applyHere m = applied m name

-- | Returns the point edits an edit makes, a settle defeating the options
-- of its question the analysis leaves possible.
expanded :: Analysis -> Edit -> Either EditError [PointEdit]
expanded _ (Rule e) = Right [e]
expanded (StaleAt _) (Settle q _) = Left (SettlesStale q.id)
expanded (Fresh s) (Settle q r) = Right [Defeat o r | o <- listedThenOther q, Map.lookup o s.verdicts == Just Possible]

-- | Returns each named point stale in one of the sets and not the other,
-- with the sets of rulings no design reaches when it is stale in the
-- model.
restaled :: Monad m => Solver m -> Model -> [Text] -> Set Text -> Set Text -> m [(Text, Maybe [Set Literal])]
restaled solver m ns was now = forM [n | n <- ns, Set.member n was /= Set.member n now] $ \n -> (,) n <$> staleness solver m n

-- | Returns each point extending the named one whose staleness differs
-- between the two models, with the sets of rulings no design reaches
-- when it is stale in the second.
extending :: Monad m => Solver m -> Model -> Model -> Text -> m [(Text, Maybe [Set Literal])]
extending solver m m' name = do
  was <- stalePoints solver m ns
  now <- stalePoints solver m' ns
  restaled solver m' ns was now
  where
    ns = descendants m.points name

data Change = Change
  { before :: Model
  , after :: Model
  , name :: Text
  , moved :: Moved
  }

-- | Returns the remarks on the point edit making the change.
remarksOn :: Monad m => Solver m -> Change -> PointEdit -> m [Remark]
remarksOn solver c (Accept o r) = (++ committedRaisers c o) <$> ruledAgain solver c (Take o) r
remarksOn solver c (Defeat o r) = ruledAgain solver c (Refuse o) (Just r)
remarksOn solver c (Reopen o) = case verdictAt c.moved.after o of
  Just v | v /= Possible -> pure . StillDecided o v <$> causes solver c.after c.name o v
  _ -> pure []
remarksOn _ c (Prefer o (Just _)) = pure [ImpossibleHere o | verdictAt c.moved.before o == Just Impossible]
remarksOn _ _ _ = pure []

ruledAgain :: Monad m => Solver m -> Change -> Literal -> Maybe Reason -> m [Remark]
ruledAgain solver c l r = case rulingFrom c.before.points c.name l of
  Just (p, old)
    | p == c.name -> pure [ReasonChanged old.reason r]
    | otherwise -> pure [AlsoRuledIn p]
  Nothing
    | verdictAt c.moved.before o == Just v -> pure . AlreadyDecided o v <$> causes solver c.before c.name o v
    | otherwise -> pure []
  where
    o = optionOf l
    v = assertedVerdict l

causes :: Monad m => Solver m -> Model -> Text -> OptionId -> Verdict -> m [Set Literal]
causes _ _ _ _ Possible = pure []
causes (Solver solving) m name o v = solving (compile m.space) (\query -> causesIn query m name o v)

causesIn :: Monad m => Query OptionId m -> Model -> Text -> OptionId -> Verdict -> m [Set Literal]
causesIn query m name o v = conflicts query (rulingsOf m.points name) (Set.singleton (opposing o v))

-- | A set of rulings giving an option its verdict, and a minimal set of the
-- space's constraints under which those rulings give it.
data Cause = Cause
  { rulings :: Set Literal
  , constraints :: [Constraint]
  }
  deriving (Eq, Show)

-- | A variable of the explanation formula: a taken option, or a
-- constraint's selector. Each constraint holds only while its selector is
-- taken.
data Selected = Picked OptionId | Active Constraint
  deriving (Eq, Ord, Show)

-- | Returns each set of rulings giving the option the verdict, with a
-- minimal set of the space's constraints under which no design holds those
-- rulings and gives the option another verdict.
explain :: Monad m => Solver m -> Space -> OptionId -> Verdict -> [Set Literal] -> m [Cause]
explain (Solver solving) space o v rss = solving (selecting space cs) (\query -> forM rss (\rs -> Cause rs <$> constraintCore query rs))
  where
    cs = Compile.constraints space
    selectors = Set.fromList [Take (Active c) | (c, _) <- cs]
    constraintCore query rs = do
      found <- core query selectors (Set.map (fmap Picked) (Set.insert (opposing o v) rs))
      pure [c | Take (Active c) <- foldMap Set.toList found]

selecting :: Space -> [(Constraint, [Set Literal])] -> Formula Selected
selecting space cs =
  Formula
    { variables = Set.map Picked (Compile.options space) <> Set.fromList [Active c | (c, _) <- cs]
    , forbidden = Set.fromList [Set.insert (Take (Active c)) (Set.map (fmap Picked) ls) | (c, lss) <- cs, ls <- lss]
    }

-- | Returns the literal no design holds when the option has the verdict.
opposing :: OptionId -> Verdict -> Literal
opposing o Forced = Refuse o
opposing o _ = Take o

-- | Returns the query over the options a design takes.
chosen :: Functor m => Set OptionId -> Query Var m -> Query OptionId m
chosen options query = Query {variables = options, design}
  where
    design held = projected (query.design (onChosen held))

projected ::
  Functor m =>
  (Set (Lit Var) -> Set Var -> m (Maybe (Set Var))) -> Set Literal -> Set OptionId -> m (Maybe (Set OptionId))
projected holding extra vs = fmap chosenOptions <$> holding (onChosen extra) (Set.map Chosen vs)

onChosen :: Set Literal -> Set (Lit Var)
onChosen = Set.map (fmap Chosen)

chosenOptions :: Set Var -> Set OptionId
chosenOptions d = Set.fromList [o | Chosen o <- Set.toList d]

committedRaisers :: Change -> OptionId -> [Remark]
committedRaisers c o =
  [ CommitsToRaiser r
  | q <- maybeToList (questionContaining c.before.space o)
  , r <- concatMap toList q.raisedBy
  , verdictAt c.moved.before r /= Just Forced
  , verdictAt c.moved.after r == Just Forced
  , isNothing (rulingFrom c.after.points c.name (Take r))
  ]

-- | Returns the moved options the applied edits account for.
statedBy :: Space -> [PointEdit] -> Moved -> Set OptionId
statedBy space pes mv = Set.fromList [o | (o, _, v) <- mv.options, isRuled o v || isImplied o v]
  where
    ruled = mapMaybe ruledLiteral pes
    isRuled o Forced = Take o `elem` ruled
    isRuled o Impossible = Refuse o `elem` ruled
    isRuled _ _ = False
    isImplied (Other q) Impossible =
      q `elem` [x | (x, _, _) <- changedQuestions mv] || Just q `elem` map (questionOf space . optionOf) ruled
    isImplied _ _ = False

withPoint :: Model -> Text -> Point -> Model
withPoint m name p = m {points = Map.insert name p m.points}

applied :: Model -> Text -> PointEdit -> Either EditError Model
applied m name e = withPoint m name <$> applyEdit m.space m.points name e

-- | A ruling supposed without a reason, or a point edit.
data Supposition
  = Suppose Literal
  | Apply PointEdit
  deriving (Eq, Show)

-- | Returns the model with the suppositions applied in order, and what
-- would move.
whatif :: Monad m => Solver m -> Model -> Text -> [Supposition] -> m (Either EditError (Model, Moved))
whatif solver m name ss = case foldM supposing m ss of
  Left e -> pure (Left e)
  Right m' -> Right . (,) m' <$> movement solver m m' name
  where
    supposing now (Suppose l) = pure (withPoint now name (withRuling now.space l Nothing (pointNamed now.points name)))
    supposing now (Apply e) = applied now name e

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

-- | Whether the user confirmed a proposed branch's rulings.
data Confirmation = Confirmed | Unconfirmed
  deriving (Eq, Show)

data AdoptError
  = UnknownBranch
  | -- | The branch is proposed and the adoption unconfirmed.
    Proposed
  | ExtendsNothing
  | -- | Holds each minimal set of the branch's rulings no design reaches.
    StaleBranch [Set Literal]
  deriving (Eq, Show)

-- | Move a branch's own rulings into the point it extends and remove the
-- branch. Points extending the branch extend that point instead. Any
-- other point extending that point which the adoption leaves stale drops
-- the fewest adopted rulings that clear its conflicts, and loses the
-- rulings it now inherits unchanged. A proposed branch needs the
-- confirmation.
adopt :: Monad m => Solver m -> Model -> Text -> Confirmation -> m (Either AdoptError Adoption)
adopt solver m name confirmation = case Map.lookup name m.points of
  Nothing -> pure (Left UnknownBranch)
  Just b | b.proposed, confirmation == Unconfirmed -> pure (Left Proposed)
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
      pure
        Adoption
          { model = fixed {points = Map.map (\x -> if x.extends == Just parent then prune fixed.points x else x) fixed.points}
          , into = parent
          , moved = mv
          , dropped
          }
    prune ps x = case x.extends of
      Nothing -> x
      Just e ->
        let inherited = effective ps e
            same l o r = case Map.lookup o inherited of
              Just (l', _, r') -> l' == l o && r' == r
              Nothing -> False
         in x
              { accept = Map.filterWithKey (\o r -> not (same Take o r)) x.accept
              , defeat = Map.filterWithKey (\o r -> not (same Refuse o r)) x.defeat
              }
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
