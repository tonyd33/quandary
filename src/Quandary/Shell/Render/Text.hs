module Quandary.Shell.Render.Text
  ( problem
  , status
  , showQuestion
  , why
  , because
  , ruled
  , settled
  , movedLines
  , Remark (..)
  , remark
  , pointError
  , refusal
  , editError
  , rulingLine
  , rulings
  , links
  , diff
  , verdictText
  , tshow
  )
where

import Data.Foldable (toList)
import Data.List (sortOn, transpose, (!?))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Analyse
import Quandary.Core.Order (decisionOrder)
import Quandary.Core.Rulings
import Quandary.Core.Source
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Shell.Storage.Files
import Quandary.Shell.Storage.Yaml (YamlIssue (..))

problem :: FilePath -> FileProblem -> Text
problem root p = Text.pack p.file <> maybe "" (\l -> ":" <> tshow l) p.line <> ": " <> message
  where
    message = case p.issue of
      Format f -> formatIssue f
      Semantic i -> issue root i

formatIssue :: FormatIssue -> Text
formatIssue NotUtf8 = "not UTF-8"
formatIssue NoFrontmatter = "no frontmatter: the file must open with a `---` line"
formatIssue UnclosedFrontmatter = "the frontmatter has no closing `---` line"
formatIssue (Yaml y) = case y of
  Syntax t -> "YAML: " <> t
  Alias -> "an alias: write the value out"
  Anchor -> "an anchor: write values out instead"
  Tag -> "a tag: every value is text"
  DuplicateKey k -> "duplicate key " <> k
  ComplexKey -> "a key that is not text"
formatIssue (UnknownKey k) = "unknown key " <> k
formatIssue (Expected s) =
  "expected " <> case s of
    TextShape -> "text"
    ListShape -> "a list"
    MappingShape -> "a mapping"

issue :: FilePath -> Issue -> Text
issue _ (BadId t) = "bad id " <> t <> ": path segments are letters, digits, - and _"
issue _ ReservedKey = "the option key other is reserved for the implicit option"
issue root (Collision t l) = "id " <> t <> " collides with " <> place root l <> ", ignoring case"
issue _ (Missing f) = "missing " <> field f
issue _ (EmptyList f) = "empty " <> field f
issue _ (BadPick t) = "pick is " <> t <> ": expected one, some or any"
issue _ (UnknownOption t) = "unknown option " <> t
issue _ (UnknownQuestion t) = "unknown question " <> t
issue _ (UnknownTarget t) = "link to unknown id " <> t
issue _ (UnknownPoint t) = "unknown point " <> t
issue root (DuplicateForbid l) = "repeats the forbid entry at " <> place root l
issue _ (BadLabel t) = "link label " <> t <> " is not kebab-case"
issue _ MissingDescription = "missing description: no prose before the next heading"
issue _ (MissingSection t) = "no ## " <> t <> " section describing the option"
issue _ (DuplicateSection t) = "a second ## " <> t <> " section"
issue _ (ExtendsCycle ns) = "extends cycle: " <> Text.intercalate " extends " ns
issue _ (AcceptedAndDefeated o) = optionName o <> " is both accepted and defeated"
issue _ (DropsNothing o) = "drops " <> optionName o <> ", which no point it extends rules"
issue _ (DropsOwn o) = "drops " <> optionName o <> ", which it rules itself"
issue _ (QuestionNotOption t) = t <> " is a question, not an option: name one of its options, or " <> t <> ".other"
issue _ (TitleMismatch o recorded now) =
  "the ruling on " <> optionName o <> " recorded the title \"" <> recorded <> "\"; the option is now \"" <> now <> "\": rule again or restore the title"
issue _ (TitleOnOther o) = "a ruling on " <> optionName o <> " records no title"
issue _ (BadRank t) = "rank " <> t <> ": expected an integer from 0 to 10"
issue _ (BadProposed t) = "proposed is " <> t <> ": expected true"
issue _ (BadUnverified t) = "unverified is " <> t <> ": expected true"
issue _ (BadGrade t) = "bad grade " <> t <> ": grades are letters, digits, - and _"
issue _ (DuplicateGrade t) = "grade " <> t <> " is listed twice"
issue root (UnknownGrade t) = "grade " <> t <> " is not one of the grades in " <> Text.pack (pathOf root ConfigDoc)
issue root NoGrades = "an assessment, but " <> Text.pack (pathOf root ConfigDoc) <> " declares no grades"

place :: FilePath -> Loc -> Text
place root l = Text.pack (pathOf root l.document) <> maybe "" (\n -> ":" <> tshow n) l.line

field :: Field -> Text
field TitleField = "title"
field PickField = "pick"
field RaisedByField = "raised-by"
field NeedsOneOfField = "needs-one-of"
field AllField = "all"
field RelField = "rel"
field ToField = "to"
field ReasonField = "reason"
field GradeField = "grade"
field GradesField = "grades"

-- | Returns the point's status, given its analysis, its options' bounds
-- and, for the table of points, the stale ones.
status :: Model -> Text -> Analysis -> Map OptionId Bounds -> Maybe (Set Text) -> Text
status m name a bs staleNames =
  Text.unlines $
    ["point " <> name]
      ++ case a of
        StaleAt cores -> "" : conflictLines m name "stale: no design holds these rulings:" "nor these:" cores
        Fresh s ->
          table "frontier" [[idText q.id, frontierOptions s q] | q <- frontier s, raised s q, null (taken s q)]
            ++ table "taken, more possible" [[idText q.id, frontierOptions s q] | q <- frontier s, not (null (taken s q))]
            ++ table "raised if taken" [[idText q.id, "by " <> raisers s q] | q <- frontier s, not (raised s q)]
            ++ listed "closed" [idText q.id <> " (" <> Text.intercalate ", " (map key (taken s q)) <> ")" | q <- inState s Closed]
            ++ listed "deferred" [idText q.id | q <- inState s Deferred]
            ++ listed "dormant" [idText q.id | q <- inState s Dormant]
      ++ maybe [] (\ss -> table "points" [[p, pointNote ss p] | p <- Map.keys m.points]) staleNames
  where
    space = m.space
    table _ [] = []
    table title rows = ["", title] ++ map ("  " <>) (columns rows)
    listed _ [] = []
    listed title items = ["", title] ++ wrapped items
    inState s st = [q | q <- Map.elems space.questions, Map.lookup q.id s.states == Just st]
    frontier s =
      [ q
      | block <- decisionOrder space (Map.keysSet (Map.filter (== Open) s.states))
      , i <- block
      , Just q <- [Map.lookup i space.questions]
      ]
    taken s q = [o | o <- choices q, Map.lookup o s.verdicts == Just Forced]
    raised s q = null q.raisedBy || any (all (\o -> Map.lookup o s.verdicts == Just Forced)) q.raisedBy
    raisers s q =
      Text.intercalate
        ", "
        [raiserText g | g <- q.raisedBy, all (\o -> Map.lookup o s.verdicts /= Just Impossible) g]
    frontierOptions s q =
      Text.intercalate "; " $
        [Text.intercalate ", " (map (\o -> Text.unwords (filter (not . Text.null) [key o, rankText ranks o, value o])) open) | not (null open)]
          ++ ["taken: " <> Text.intercalate ", " (map key os) | let os = taken s q, not (null os)]
      where
        possible = [o | o <- byPreference m name q, Map.lookup o s.verdicts == Just Possible]
        open = case filter (/= Other q.id) possible of
          [_] -> possible
          [] -> possible
          listed' -> listed'
    ranks = preferenceAt m.points name
    value o = case Map.lookup o bs of
      Just b | b.lo > 0 || b.hi < topGrade space -> boundsText space b
      _ -> ""
    pointNote ss p =
      Text.intercalate ", " $
        [if Set.member p ss then "stale" else "fresh"]
          ++ ["proposed" | (pointNamed m.points p).proposed]
          ++ case (pointNamed m.points p).extends of
            Nothing -> []
            Just e
              | same p e -> ["extends " <> e <> " and rules nothing else"]
              | otherwise -> ["extends " <> e]
    same p e = rulingsOf m.points p == rulingsOf m.points e && deferredAt m.points p == deferredAt m.points e

-- | Returns the bounds by grade name, one name when they meet.
boundsText :: Space -> Bounds -> Text
boundsText space b
  | b.lo == b.hi = gradeName space b.lo
  | otherwise = gradeName space b.lo <> ".." <> gradeName space b.hi

-- | Returns the name of the grade at the position.
gradeName :: Space -> Int -> Text
gradeName space i = fromMaybe "" (space.grades !? i)

-- | Returns the rows in columns, each column padded to two past its widest
-- cell.
columns :: [[Text]] -> [Text]
columns rows = [Text.stripEnd (mconcat (zipWith (\w c -> Text.justifyLeft w ' ' c) widths r)) | r <- rows]
  where
    widths = map ((+ 2) . maximum . map Text.length) (transpose rows)

-- | Returns the options a raiser takes together, joined by `with`.
raiserText :: NonEmpty OptionId -> Text
raiserText g = Text.intercalate " with " (map optionName (toList g))

-- | Returns the option's rank in brackets, or nothing when unranked.
rankText :: Map OptionId Int -> OptionId -> Text
rankText ranks o = maybe "" (\n -> "[" <> tshow n <> "]") (Map.lookup o ranks)

-- | Returns the items comma-separated, in indented lines of at most 80
-- columns.
wrapped :: [Text] -> [Text]
wrapped = map (("  " <>) . Text.intercalate ", ") . go
  where
    go [] = []
    go (x : xs) = let (line, rest) = fill (Text.length x) [x] xs in line : go rest
    fill _ acc [] = (reverse acc, [])
    fill n acc (y : ys)
      | n + 2 + Text.length y > 78 = (reverse acc, y : ys)
      | otherwise = fill (n + 2 + Text.length y) (y : acc) ys

-- | Returns an option's key: its id without the directory, `other` for an
-- implicit other.
key :: OptionId -> Text
key (Listed (Id t)) = snd (Text.breakOnEnd "/" t)
key (Other _) = "other"

-- | Returns the question's options by preference: ranked first, highest
-- rank first, then as listed, the implicit other last.
byPreference :: Model -> Text -> Question -> [OptionId]
byPreference m name q = sortOn (\o -> Down (Map.lookup o ranks)) (listedThenOther q)
  where
    ranks = preferenceAt m.points name

-- | Returns the options' lines in columns: standing, id, then title, rank
-- and reason.
optionLines :: Model -> Text -> Maybe Standing -> Map OptionId Bounds -> [OptionId] -> [Text]
optionLines m name s bs os = zipWith (\o line -> Text.justifyLeft 12 ' ' (markText (markOf o)) <> line) os (columns (map row os))
  where
    rulingsHere = effective m.points name
    ranks = preferenceAt m.points name
    markOf o = mark (Map.lookup o rulingsHere) (s >>= Map.lookup o . (.verdicts))
    row o = [optionName o, Text.intercalate "  " (filter (not . Text.null) [title, value, rankText ranks o, note])]
      where
        note = maybe "" (\(_, p, r) -> from p r) (Map.lookup o rulingsHere)
        from p r = Text.intercalate ": " (filter (not . Text.null) [if p == name then "" else "in " <> p, fromMaybe "" r.reason])
        title = fromMaybe "" (titleOf m.space o)
        value = maybe "" (("value " <>) . boundsText m.space) (Map.lookup o bs)

-- | An option's standing at a point: its ruling there, or else its verdict.
data Mark = Accepted | Defeated | Judged (Maybe Verdict)
  deriving (Eq)

mark :: Maybe (Literal, Text, Ruling) -> Maybe Verdict -> Mark
mark ruling v = case ruling of
  Just (Take _, _, _) -> Accepted
  Just (Refuse _, _, _) -> Defeated
  Nothing -> Judged v

markText :: Mark -> Text
markText Accepted = "accepted"
markText Defeated = "defeated"
markText (Judged v) = maybe "" verdictText v

verdictText :: Verdict -> Text
verdictText Forced = "forced"
verdictText Possible = "possible"
verdictText Impossible = "impossible"

stateText :: QuestionState -> Text
stateText Open = "open"
stateText Closed = "closed"
stateText Deferred = "deferred"
stateText Dormant = "dormant"

showQuestion :: Model -> Text -> Analysis -> Map OptionId Bounds -> Question -> Text
showQuestion m name a bs q =
  Text.unlines $
    [idText q.id <> ": " <> q.title <> state, shape, "", q.description, ""]
      ++ concat [line : graded o ++ dominated o ++ raises o | (o, line) <- zip os (optionLines m name (standing a) bs os)]
  where
    state = case a of
      Fresh s -> maybe "" (\st -> " [" <> stateText st <> "]") (Map.lookup q.id s.states)
      StaleAt _ -> " [stale point]"
    shape =
      "pick: " <> (case q.pick of One -> "one"; Some -> "some"; Any -> "any") <> case q.raisedBy of
        [] -> ", a root"
        rs -> ", raised by " <> Text.intercalate ", " (map raiserText rs)
    os = byPreference m name q
    graded o =
      [ "  graded " <> a'.grade <> (if a'.unverified then ", unverified" else "") <> maybe "" (": " <>) a'.why
      | Listed i <- [o]
      , Just (_, opt) <- [listed i]
      , Just a' <- [opt.assessment]
      ]
    listed = optionAt m.space
    dominated o = case (Map.lookup o bs, sortOn (Down . (.lo) . snd) [(p, b) | p <- os, p /= o, Just b <- [Map.lookup p bs]]) of
      (Just b, (p, best) : _)
        | b.hi < best.lo ->
            ["  dominated: at best " <> gradeName m.space b.hi <> ", below " <> optionName p <> " at worst " <> gradeName m.space best.lo]
      _ -> []
    raises o = ["  raises " <> Text.intercalate ", " rs | let rs = raisedBy o, not (null rs)]
    raisedBy o =
      [ idText r.id <> with
      | r <- Map.elems m.space.questions
      , g <- r.raisedBy
      , o `elem` g
      , let with = case filter (/= o) (toList g) of
              [] -> ""
              others -> " (with " <> Text.intercalate ", " (map optionName others) <> ")"
      ]

why :: Model -> Text -> OptionId -> Verdict -> [Set Literal] -> Text
why m name o v cores = case v of
  Possible -> optionName o <> " is possible\n"
  _ -> Text.unlines (because m name (optionName o <> " is " <> verdictText v) v o cores)

-- | Returns the lines saying which sets of rulings give the option its
-- verdict, under the given opening.
because :: Model -> Text -> Text -> Verdict -> OptionId -> [Set Literal] -> [Text]
because _ _ opening _ _ [] = [opening]
because m name opening v o cores
  | any Set.null cores = ["no valid design " <> verb <> " " <> optionName o <> ", whatever the rulings"]
  | otherwise = conflictLines m name (opening <> " because of:") "and because of:" cores
  where
    verb = case v of
      Forced -> "refuses"
      _ -> "takes"

rulingLine :: Model -> Text -> Literal -> Text
rulingLine m name l = verb <> " " <> optionName (optionOf l) <> source
  where
    verb = case l of
      Take _ -> "accept"
      Refuse _ -> "defeat"
    source = case rulingFrom m.points name l of
      Just (p, r) -> " (" <> p <> maybe "" (": " <>) r.reason <> ")"
      Nothing -> ""

-- | Returns the moves, one line per question touched, rendered against
-- the model they lead to. A question's implicit other is named only when
-- it is forced or is all that moved.
movedLines :: Model -> Text -> Moved -> [Text]
movedLines m name mv =
  map question touched
    ++ concat [conflictLines m name "the point is now stale, because of:" "and because of:" cores | StaleAt cores <- [mv.after], not (stale mv.before)]
    ++ ["the point is no longer stale" | stale mv.before, not (stale mv.after)]
    ++ concat [branch p core | (p, core) <- mv.branches]
    ++ ["nothing moved" | null mv.options, onlyOptionsMoved mv]
  where
    byQuestion = Map.fromListWith (flip (++)) [(q, [(o, a)]) | (o, _, a) <- mv.options, Just q <- [questionOf m.space o]]
    states = Map.fromList [(q, (b, a)) | (q, b, a) <- mv.questions]
    touched = Set.toList (Map.keysSet byQuestion <> Map.keysSet states)
    question q = idText q <> change <> detail
      where
        change = maybe "" (\(b, a) -> ": " <> stateText b <> " -> " <> stateText a) (Map.lookup q states)
        changes = Map.findWithDefault [] q byQuestion
        shown = case filter (\(o, a) -> o /= Other q || a == Forced) changes of
          [] -> changes
          cs -> cs
        groups = [Text.intercalate ", " [key o | (o, a) <- shown, a == v] <> " " <> verdictText v | v <- [Forced, Possible, Impossible], any ((== v) . snd) shown]
        detail
          | fmap snd (Map.lookup q states) == Just Dormant || null groups = ""
          | otherwise = " (" <> Text.intercalate "; " groups <> ")"
    branch p Nothing = ["point " <> p <> " is no longer stale"]
    branch p (Just cores) =
      conflictLines m p ("point " <> p <> " is now stale, because of:") "and because of:" cores
        ++ ["reopen there drops what it inherits" | any (inheritedAt m.points p) (concatMap Set.toList cores)]

-- | Returns the line naming an edit as done.
ruled :: Edit -> Text
ruled (Accept o _) = "accepted " <> optionName o
ruled (Defeat (Other q) _) = "exhausted " <> idText q
ruled (Defeat o _) = "defeated " <> optionName o
ruled (Reopen o) = "reopened " <> optionName o
ruled (Undefer q) = "reopened " <> idText q
ruled (Defer q) = "deferred " <> idText q
ruled (Prefer o (Just n)) = "ranked " <> optionName o <> " " <> tshow n
ruled (Prefer o Nothing) = "unranked " <> optionName o

-- | Returns the line naming a question as settled, with the options it
-- defeated.
settled :: Id -> [OptionId] -> Text
settled q [] = "settled " <> idText q
settled q os = "settled " <> idText q <> " (" <> Text.intercalate ", " (map key os) <> " defeated)"

refusal :: Model -> Text -> Refusal -> Text
refusal _ _ (Refused e) = editError e
refusal m name (WouldBeStale cores) =
  Text.unlines $
    conflictLines m name "refused: no design would reach the point, because of:" "and because of:" cores
      ++ [idText q <> " is pick: one" | q <- pickOne]
      ++ ["reopen drops an inherited ruling at this point" | any (inheritedAt m.points name) ls]
  where
    ls = concatMap Set.toList cores
    pickOne =
      Set.toList . Set.fromList $
        [ q.id
        | c <- cores
        , (i, n) <- Map.toList (Map.fromListWith (+) [(i, 1 :: Int) | Take o <- Set.toList c, Just i <- [questionOf m.space o]])
        , n > 1
        , Just q <- [Map.lookup i m.space.questions]
        , q.pick == One
        ]

-- | Returns each set of rulings under its heading, the first heading
-- before the first set and the second before each later one.
conflictLines :: Model -> Text -> Text -> Text -> [Set Literal] -> [Text]
conflictLines m name first next cores =
  concat [h : map (("  " <>) . rulingLine m name) (Set.toList c) | (h, c) <- zip (first : repeat next) cores]

editError :: EditError -> Text
editError (NotRuled o) = optionName o <> " has no ruling to reopen\n"
editError (NotDeferred q) = idText q <> " is not deferred\n"

-- | Returns the point's rulings, its own and inherited, each with the
-- point it comes from.
rulings :: Model -> Text -> Text
rulings m name =
  Text.unlines $
    ["point " <> name <> maybe "" (" extends " <>) (pointNamed m.points name).extends, ""]
      ++ [rulingLine m name l | (l, _, _) <- sortOn order (Map.elems (effective m.points name))]
      ++ ["drop " <> optionName o <> " (" <> p.name <> ")" | p <- chain, o <- Set.toList p.drop]
      ++ ["defer " <> idText q <> " (" <> p.name <> ")" | p <- chain, q <- Set.toList p.defer]
      ++ ["prefer " <> optionName o <> " " <> tshow n <> " (" <> from o <> ")" | (o, n) <- Map.toList (preferenceAt m.points name)]
  where
    chain = lineage m.points name
    order (l, _, _) = (case l of Take _ -> 0 :: Int; Refuse _ -> 1, optionOf l)
    from o = case [p.name | p <- chain, Map.member o p.prefer] of
      p : _ -> p
      [] -> name

-- | Returns each link label in the space with how many links use it, most
-- used first.
links :: Space -> Text
links space = Text.unlines (["link labels"] ++ if null counts then ["  none"] else map ("  " <>) (columns [[rel, tshow n] | (rel, n) <- counts]))
  where
    counts = sortOn (\(rel, n) -> (Down n, rel)) (Map.toList (Map.fromListWith (+) [(l.rel, 1 :: Int) | l <- every]))
    every =
      concat [q.links ++ concatMap (.links) q.options | q <- Map.elems space.questions]
        ++ concatMap (.links) (Map.elems space.notes)

-- | Returns, for each question whose state or options differ between the
-- two points, the question's state and each differing option's standing
-- at both.
diff :: Model -> (Text, Standing) -> (Text, Standing) -> Text
diff m (a, sa) (b, sb) =
  Text.unlines $
    ["diff " <> a <> " " <> b, ""] ++ case rows of
      [] -> ["no difference"]
      _ -> columns (["", a, b] : rows)
  where
    rows =
      concat
        [ [idText q.id, stateAt sa q, stateAt sb q] : [["  " <> optionName o, markText x, markText y] | (o, x, y) <- options]
        | q <- Map.elems m.space.questions
        , let options = [(o, x, y) | o <- choices q, let x = markAt ra sa o, let y = markAt rb sb o, x /= y, o /= Other q.id || isRuled x || isRuled y]
        , stateAt sa q /= stateAt sb q || not (null options)
        ]
    ra = effective m.points a
    rb = effective m.points b
    stateAt s q = maybe "" stateText (Map.lookup q.id s.states)
    isRuled (Judged _) = False
    isRuled _ = True
    markAt rs s o = mark (Map.lookup o rs) (Map.lookup o s.verdicts)

tshow :: Show a => a -> Text
tshow = Text.pack . show

data Remark
  = AlreadyDecided OptionId Verdict [Set Literal]
  | StillDecided OptionId Verdict [Set Literal]
  | ReasonChanged (Maybe Text) (Maybe Text)
  | AlsoRuledIn Text
  | CommitsToRaiser OptionId
  | ImpossibleHere OptionId
  deriving (Eq, Show)

-- | Returns a remark's lines, rendered against the model before the edit or
-- after it, whichever the remark is about.
remark :: Model -> Model -> Text -> Remark -> [Text]
remark before _ name (AlreadyDecided o v cores) =
  because before name (optionName o <> " was already " <> verdictText v) v o shown
    ++ ["and " <> tshow (length rest) <> " more: quandary why " <> optionName o | not (null rest)]
  where
    (shown, rest) = splitAt 1 cores
remark _ after name (StillDecided o v cores) = because after name (optionName o <> " is still " <> verdictText v) v o cores
remark _ _ _ (ReasonChanged old new) = ["reason: " <> reasonText old <> " -> " <> reasonText new]
remark _ _ _ (AlsoRuledIn p) = ["also ruled so in " <> p]
remark _ _ _ (CommitsToRaiser r) = ["commits to raiser " <> optionName r <> ", which nothing else rules"]
remark _ _ _ (ImpossibleHere o) = [optionName o <> " is impossible at this point"]

reasonText :: Maybe Text -> Text
reasonText = fromMaybe "none"

pointError :: PointError -> Text
pointError (PointExists p) = "point " <> p <> " exists"
pointError (NoSuchPoint p) = "unknown point " <> p
pointError RemovesCurrent = "current cannot be removed"
pointError (ExtendedBy child p) = "point " <> child <> " extends " <> p
