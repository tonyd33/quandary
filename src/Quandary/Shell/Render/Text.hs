module Quandary.Shell.Render.Text
  ( problem
  , status
  , showQuestion
  , showOption
  , optionTitle
  , optionFacts
  , rulingVerb
  , rulingSource
  , rulingSummary
  , valueText
  , choiceNote
  , pickText
  , assessmentNote
  , gradeName
  , designVerb
  , why
  , because
  , ruled
  , movedLines
  , whatifResult
  , implicationLines
  , stepLines
  , pointError
  , badRank
  , refusal
  , editError
  , rulingLine
  , rulings
  , links
  , diff
  , verdictText
  , stateText
  , tshow
  , questionTitle
  , stateSuffix
  , questionShape
  , rowMark
  , rowDetail
  , rowNotes
  , raisedTitle
  , coRaiserText
  , staleLines
  , groupTitle
  , entryDetail
  , pointNote
  , optionLines
  , key
  )
where

import Data.Foldable (toList)
import Data.List (sortOn, transpose, (!?))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Analyse
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Rulings
import Quandary.Core.Source
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
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
  "the ruling on "
    <> optionName o
    <> " recorded the title \""
    <> recorded
    <> "\"; the option is now \""
    <> now
    <> "\": rule again or restore the title"
issue _ (TitleOnOther o) = "a ruling on " <> optionName o <> " records no title"
issue _ (BadRank t) = badRank t
issue _ EmptyReason = "empty reason"
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
-- and, for the table of points, the points.
status :: Model -> Text -> Analysis -> Map OptionId Bounds -> Maybe [PointView] -> Text
status m name a bs points =
  Text.unlines $
    ["point " <> name]
      ++ case a of
        StaleAt cores -> "" : staleLines m name cores
        Fresh s -> concatMap section (statusGroups m name bs s)
      ++ maybe [] (\pvs -> table "points" [[pv.point.name, pointNote pv] | pv <- pvs]) points
  where
    table _ [] = []
    table title rows = ["", title] ++ map ("  " <>) (columns rows)
    listed _ [] = []
    listed title items = ["", title] ++ wrapped items
    detail e = fromMaybe "" (entryDetail m.space e.detail)
    tabled g = table (groupTitle g.kind) [[idText e.question.id, detail e] | e <- g.entries]
    bare g = listed (groupTitle g.kind) [idText e.question.id | e <- g.entries]
    section g = case g.kind of
      Frontier -> tabled g
      TakenMore -> tabled g
      RaisedIfTaken -> tabled g
      ClosedGroup -> listed (groupTitle g.kind) [idText e.question.id <> " (" <> detail e <> ")" | e <- g.entries]
      DeferredGroup -> bare g
      DormantGroup -> bare g

groupTitle :: GroupKind -> Text
groupTitle Frontier = "frontier"
groupTitle TakenMore = "taken, more possible"
groupTitle RaisedIfTaken = "raised if taken"
groupTitle ClosedGroup = "closed"
groupTitle DeferredGroup = "deferred"
groupTitle DormantGroup = "dormant"

-- | Returns what status shows beside a question.
entryDetail :: Space -> Detail -> Maybe Text
entryDetail space (Choosing cs) = Just (Text.unwords (choiceChunks space cs))
entryDetail _ (RaisedIf rs) = Just ("by " <> Text.intercalate ", " (map raiserText rs))
entryDetail _ (Took []) = Nothing
entryDetail _ (Took os) = Just (Text.intercalate ", " (map key os))
entryDetail _ NoDetail = Nothing

pointNote :: PointView -> Text
pointNote pv = Text.intercalate ", " (pointNotes pv)

-- | Returns the point's state, whether it is proposed, and the point it
-- extends.
pointNotes :: PointView -> [Text]
pointNotes pv =
  [if pv.stale then "stale" else "fresh"]
    ++ ["proposed" | pv.point.proposed]
    ++ case pv.point.extends of
      Nothing -> []
      Just e
        | pv.rulesNothingElse -> ["extends " <> e <> " and rules nothing else"]
        | otherwise -> ["extends " <> e]

-- | Returns the possible options with their ranks and bounds, then the
-- taken ones, punctuated to be joined by spaces.
choiceChunks :: Space -> Choices -> [Text]
choiceChunks space cs = case (map (choiceText space) cs.possible, map key cs.taken) of
  (os, []) -> commas os
  ([], ts) -> takenChunks ts
  (os, ts) -> closedWith ";" (commas os) ++ takenChunks ts

-- | Returns the option's key, rank and bounds, the bounds only when they
-- leave out some grade.
choiceText :: Space -> Choice -> Text
choiceText space c = Text.unwords (filter (not . Text.null) [key c.option, choiceNote space c])

-- | Returns the option's rank and bounds, the bounds only when they leave
-- out some grade.
choiceNote :: Space -> Choice -> Text
choiceNote space c = Text.unwords (filter (not . Text.null) [rankText c.rank, value])
  where
    value = case c.bounds of
      Just b | b.lo > 0 || b.hi < topGrade space -> boundsText space b
      _ -> ""

takenChunks :: [Text] -> [Text]
takenChunks ts = case commas ts of
  t : rest -> ("taken: " <> t) : rest
  [] -> []

-- | Append the text to the last chunk.
closedWith :: Text -> [Text] -> [Text]
closedWith _ [] = []
closedWith end [x] = [x <> end]
closedWith end (x : xs) = x : closedWith end xs

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
rankText :: Maybe Rank -> Text
rankText = maybe "" (\n -> "[" <> tshow (rankValue n) <> "]")

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

-- | Returns the questions the accepted options raise, each with its
-- co-raisers and the bounds of its possible options, then its taken ones.
implicationLines :: Model -> Text -> Standing -> [Implication] -> [Text]
implicationLines _ _ _ [] = []
implicationLines m name s is = "" : "raises" : concatMap implication is
  where
    implication i = case i.values of
      Nothing -> ("  " <> raisedTitle i.raising <> " [dormant]") : coRaiserLines "  " i.raising
      Just bs ->
        ("  " <> raisedTitle i.raising)
          : coRaiserLines "  " i.raising
          ++ filled "    " "      " (choiceChunks m.space (choicesAt m name bs s i.raising.question))

raisedTitle :: Raising -> Text
raisedTitle r = idText r.question.id <> ": " <> r.question.title

-- | Returns the lines naming the options that raise the question alongside,
-- none when the options raise it alone, under a line indented as given.
coRaiserLines :: Text -> Raising -> [Text]
coRaiserLines indent r = maybe [] (\(lead, cs) -> filled (indent <> "  " <> lead) (indent <> "    ") (commas cs)) (coRaisers r)

-- | Returns the options that raise the question alongside on one line.
coRaiserText :: Raising -> Maybe Text
coRaiserText r = (\(lead, cs) -> lead <> Text.intercalate ", " cs) <$> coRaisers r

coRaisers :: Raising -> Maybe (Text, [Text])
coRaisers r = case r.with of
  [] -> Nothing
  [g] -> Just ("with ", [coRaiser g])
  gs -> Just ("with any of ", map coRaiser gs)

coRaiser :: NonEmpty OptionId -> Text
coRaiser (o :| []) = optionName o
coRaiser g = "(" <> raiserText g <> ")"

-- | Returns the chunks space-separated, breaking before any chunk that
-- would pass column 80, the first line opening with the first prefix and
-- the rest with the second.
filled :: Text -> Text -> [Text] -> [Text]
filled _ _ [] = []
filled first next (c : cs) = go (first <> c) cs
  where
    go line [] = [line]
    go line (d : ds)
      | Text.length line + 1 + Text.length d > 80 = line : go (next <> d) ds
      | otherwise = go (line <> " " <> d) ds

-- | Returns the items with a comma after each but the last.
commas :: [Text] -> [Text]
commas [] = []
commas [x] = [x]
commas (x : xs) = (x <> ",") : commas xs

-- | Returns an option's key: its id without the directory, `other` for an
-- implicit other.
key :: OptionId -> Text
key (Listed (Id t)) = snd (Text.breakOnEnd "/" t)
key (Other _) = "other"

-- | Returns the options' lines in columns: standing, id, then title, rank
-- and reason.
optionLines :: Space -> Text -> [OptionRow] -> [Text]
optionLines space name rows =
  zipWith
    (\r line -> Text.justifyLeft 12 ' ' (rowMark r) <> line)
    rows
    (columns [[optionName r.option, rowDetail space name r] | r <- rows])

-- | Returns the option's ruling, or else its verdict.
rowMark :: OptionRow -> Text
rowMark r = markText (mark r.ruling r.verdict)

-- | Returns the option's title, bounds and rank, and its ruling's point
-- and reason.
rowDetail :: Space -> Text -> OptionRow -> Text
rowDetail space name r = Text.intercalate "  " (filter (not . Text.null) [fromMaybe "" r.title, valueText space r, note])
  where
    note = maybe "" (rulingNote name) r.ruling

-- | Returns the option's bounds and rank.
valueText :: Space -> OptionRow -> Text
valueText space r = Text.intercalate "  " (filter (not . Text.null) [value, rankText r.rank])
  where
    value = maybe "" (("value " <>) . boundsText space) r.bounds

-- | Returns the ruling's reason, after the point it comes from unless that
-- is the point given.
rulingNote :: Text -> (Literal, Text, Ruling) -> Text
rulingNote name (_, p, given)
  | p == name = maybe "" reasonText given.reason
  | otherwise = Text.intercalate ": " (("in " <> p) : map reasonText (maybeToList given.reason))

-- | Returns the option's grade and, when dominated, its dominator.
rowNotes :: Space -> OptionRow -> [Text]
rowNotes space r = maybeToList (assessmentNote <$> r.assessment) ++ maybeToList (dominationNote space <$> r.dominated)

assessmentNote :: Assessment -> Text
assessmentNote a = "graded " <> a.grade <> (if a.unverified then ", unverified" else "") <> maybe "" (": " <>) a.why

dominationNote :: Space -> Domination -> Text
dominationNote space d =
  "dominated: at best "
    <> gradeName space d.atBest
    <> ", below "
    <> optionName d.by
    <> " at worst "
    <> gradeName space d.byAtWorst

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
stateText (Open _) = "open"
stateText Closed = "closed"
stateText Deferred = "deferred"
stateText Dormant = "dormant"

-- | Returns the question in full.
showQuestion :: Space -> QuestionView -> Text
showQuestion space v =
  Text.unlines $
    [questionTitle v.analysis v.question, questionShape v.question, "", v.question.description, ""]
      ++ concat
        [ line : map ("  " <>) (rowNotes space r) ++ raisesLines "  " r
        | (r, line) <- zip v.rows (optionLines space v.point v.rows)
        ]

raisesLines :: Text -> OptionRow -> [Text]
raisesLines indent r = concat [(indent <> "raises " <> raisedTitle rq) : coRaiserLines indent rq | rq <- r.raises]

-- | Returns the option in full: its question, ruling, why it is forced or
-- impossible, bounds, rank, grade, dominator, description and the
-- questions it raises.
showOption :: Model -> OptionView -> Text
showOption m v =
  Text.unlines $
    [optionTitle v, "in " <> questionTitle v.analysis v.question]
      ++ optionFacts m v
      ++ paragraph [v.description | not (Text.null v.description)]
      ++ paragraph (raisesLines "" v.row)
  where
    paragraph [] = []
    paragraph ls = "" : ls

-- | Returns the option's id and title with its ruling, or else its verdict.
optionTitle :: OptionView -> Text
optionTitle v = optionName v.row.option <> maybe "" (": " <>) v.row.title <> state
  where
    state = case v.analysis of
      Fresh _ -> " [" <> rowMark v.row <> "]"
      StaleAt _ -> " [stale point]"

-- | Returns the option's ruling with its point and reason, every set of
-- rulings making it forced or impossible, its bounds and rank, grade and
-- dominator.
optionFacts :: Model -> OptionView -> [Text]
optionFacts m v =
  maybe [] (pure . rulingSummary v.point) v.row.ruling
    ++ maybe [] (\verdict -> because m v.point (verdictText verdict) verdict v.row.option v.causes) causeVerdict
    ++ filter (not . Text.null) [valueText m.space v.row]
    ++ rowNotes m.space v.row
  where
    causeVerdict
      | null v.causes = Nothing
      | otherwise = v.row.verdict

-- | Returns the ruling's verb, the point it comes from unless that is the
-- point given, and its reason.
rulingSummary :: Text -> (Literal, Text, Ruling) -> Text
rulingSummary name (l, p, given) =
  markText (mark (Just (l, p, given)) Nothing)
    <> (if p == name then "" else " in " <> p)
    <> maybe "" ((": " <>) . reasonText) given.reason

-- | Returns the question's id and title with its state.
questionTitle :: Analysis -> Question -> Text
questionTitle a q = idText q.id <> ": " <> q.title <> stateSuffix a q

-- | Returns the question's state in brackets.
stateSuffix :: Analysis -> Question -> Text
stateSuffix a q = case a of
  Fresh s -> maybe "" (\st -> " [" <> stateText st <> "]") (Map.lookup q.id s.states)
  StaleAt _ -> " [stale point]"

-- | Returns the question's pick and raisers.
questionShape :: Question -> Text
questionShape q =
  "pick: " <> pickText q.pick <> case q.raisedBy of
    [] -> ", a root"
    rs -> ", raised by " <> Text.intercalate ", " (map raiserText rs)

pickText :: Pick -> Text
pickText One = "one"
pickText Some = "some"
pickText Any = "any"

why :: Model -> Text -> OptionId -> Verdict -> [Set Literal] -> Text
why m name o v cores = case v of
  Possible -> optionName o <> " is possible\n"
  _ -> Text.unlines (because m name (optionName o <> " is " <> verdictText v) v o cores)

-- | Returns the lines saying which sets of rulings give the option its
-- verdict, under the given opening.
because :: Model -> Text -> Text -> Verdict -> OptionId -> [Set Literal] -> [Text]
because _ _ opening _ _ [] = [opening]
because m name opening v o cores
  | any Set.null cores = ["no valid design " <> designVerb v <> " " <> optionName o <> ", whatever the rulings"]
  | otherwise = conflictLines m name (opening <> " because of:") "and because of:" cores

-- | Returns what no valid design does to an option with the verdict.
designVerb :: Verdict -> Text
designVerb Forced = "refuses"
designVerb _ = "takes"

rulingLine :: Model -> Text -> Literal -> Text
rulingLine m name l = rulingVerb l <> " " <> optionName (optionOf l) <> rulingSource m name l

rulingVerb :: Literal -> Text
rulingVerb (Take _) = "accept"
rulingVerb (Refuse _) = "defeat"

-- | Returns the point the ruling comes from with its reason, in brackets.
rulingSource :: Model -> Text -> Literal -> Text
rulingSource m name l = case rulingFrom m.points name l of
  Just (p, r) -> " (" <> p <> maybe "" ((": " <>) . reasonText) r.reason <> ")"
  Nothing -> ""

-- | Returns the moves, one line per question touched, rendered against
-- the model they lead to. A question's implicit other is named only when
-- it is forced or is all that moved.
movedLines :: Model -> Text -> Moved -> [Text]
movedLines m name mv =
  map question touched
    ++ concat
      [ conflictLines m name "the point is now stale, because of:" "and because of:" cores
      | StaleAt cores <- [mv.after]
      , not (stale mv.before)
      ]
    ++ ["the point is no longer stale" | stale mv.before, not (stale mv.after)]
    ++ concat [branch p core | (p, core) <- mv.branches]
    ++ ["nothing moved" | null mv.options, onlyOptionsMoved mv]
  where
    byQuestion = Map.fromListWith (flip (++)) [(q, [(o, a)]) | (o, _, a) <- mv.options, Just q <- [questionOf m.space o]]
    states = Map.fromList [(q, (b, a)) | (q, b, a) <- changedQuestions mv]
    touched = Set.toList (Map.keysSet byQuestion <> Map.keysSet states)
    question q = idText q <> change <> detail
      where
        change = maybe "" (\(b, a) -> ": " <> stateText b <> " -> " <> stateText a) (Map.lookup q states)
        changes = Map.findWithDefault [] q byQuestion
        shown = case filter (\(o, a) -> o /= Other q || a == Forced) changes of
          [] -> changes
          cs -> cs
        groups =
          [ Text.intercalate ", " [key o | (o, a) <- shown, a == v] <> " " <> verdictText v
          | v <- [Forced, Possible, Impossible]
          , any ((== v) . snd) shown
          ]
        detail
          | fmap snd (Map.lookup q states) == Just Dormant || null groups = ""
          | otherwise = " (" <> Text.intercalate "; " groups <> ")"
    branch p Nothing = ["point " <> p <> " is no longer stale"]
    branch p (Just cores) =
      conflictLines m p ("point " <> p <> " is now stale, because of:") "and because of:" cores
        ++ ["reopen there drops what it inherits" | any (inheritedAt m.points p) (concatMap Set.toList cores)]

-- | Returns why the suppositions cannot apply, or the moves and then the
-- questions the supposed acceptances raise.
whatifResult :: Text -> Either EditError Whatif -> Either Text [Text]
whatifResult _ (Left e) = Left (editError e)
whatifResult name (Right w) =
  Right
    ( movedLines w.model name w.moved
        ++ maybe [] (\s -> implicationLines w.model name s w.implications) (standing w.moved.after)
    )

-- | Holds when no question changed state and no branch or staleness
-- moved.
onlyOptionsMoved :: Moved -> Bool
onlyOptionsMoved mv = null (changedQuestions mv) && null mv.branches && stale mv.after == stale mv.before

-- | Returns the line naming an edit as done, given the edits it was
-- applied as.
ruled :: Edit -> [PointEdit] -> Text
ruled (Rule e) _ = done e
ruled (Settle question _) applied = case [o | Defeat o _ <- applied] of
  [] -> "settled " <> idText question.id
  os -> "settled " <> idText question.id <> " (" <> Text.intercalate ", " (map key os) <> " defeated)"

done :: PointEdit -> Text
done (Accept o _) = "accepted " <> optionName o
done (Defeat (Other q) _) = "exhausted " <> idText q
done (Defeat o _) = "defeated " <> optionName o
done (Reopen o) = "reopened " <> optionName o
done (Undefer q) = "reopened " <> idText q
done (Defer q) = "deferred " <> idText q
done (Prefer o (Just n)) = "ranked " <> optionName o <> " " <> tshow (rankValue n)
done (Prefer o Nothing) = "unranked " <> optionName o

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

staleLines :: Model -> Text -> [Set Literal] -> [Text]
staleLines m name = conflictLines m name "stale: no design holds these rulings:" "nor these:"

-- | Returns each set of rulings under its heading, the first heading
-- before the first set and the second before each later one.
conflictLines :: Model -> Text -> Text -> Text -> [Set Literal] -> [Text]
conflictLines m name first next cores =
  concat [h : map (("  " <>) . rulingLine m name) (Set.toList c) | (h, c) <- zip (first : repeat next) cores]

editError :: EditError -> Text
editError (NotRuled o) = optionName o <> " has no ruling to reopen\n"
editError (NotDeferred q) = idText q <> " is not deferred\n"
editError (SettlesStale q) = idText q <> " cannot be settled: the point is stale\n"

-- | Returns the point's rulings, its own and inherited, each with the
-- point it comes from.
rulings :: Model -> Text -> Text
rulings m name =
  Text.unlines $
    ["point " <> name <> maybe "" (" extends " <>) (pointNamed m.points name).extends, ""]
      ++ [rulingLine m name l | (l, _, _) <- sortOn order (Map.elems (effective m.points name))]
      ++ ["drop " <> optionName o <> " (" <> p.name <> ")" | p <- chain, o <- Set.toList p.drop]
      ++ ["defer " <> idText q <> " (" <> p.name <> ")" | p <- chain, q <- Set.toList p.defer]
      ++ [ "prefer " <> optionName o <> " " <> tshow (rankValue n) <> " (" <> from o <> ")"
         | (o, n) <- Map.toList (preferenceAt m.points name)
         ]
  where
    chain = lineage m.points name
    order (l, _, _) = (case l of Take _ -> 0 :: Int; Refuse _ -> 1, optionOf l)
    from o = case [p.name | p <- chain, Map.member o p.prefer] of
      p : _ -> p
      [] -> name

-- | Returns each link label in the space with how many links use it, most
-- used first.
links :: Space -> Text
links space =
  Text.unlines
    (["link labels"] ++ if null counts then ["  none"] else map ("  " <>) (columns [[rel, tshow n] | (rel, n) <- counts]))
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
        , let options =
                [ (o, x, y)
                | o <- choices q
                , let x = markAt ra sa o
                , let y = markAt rb sb o
                , x /= y
                , o /= Other q.id || isRuled x || isRuled y
                ]
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

-- | Returns the lines naming a step, what it moved that it does not
-- account for, and its remarks.
stepLines :: Text -> Step -> [Text]
stepLines name step = ruled step.edit step.applied : map ("  " <>) (unstated ++ remarks)
  where
    mv = step.moved
    others = [x | x@(o, _, _) <- mv.options, Set.notMember o step.stated]
    unstated
      | not (null mv.options) && null others && onlyOptionsMoved mv = []
      | otherwise = movedLines step.model name mv {Analyse.options = others}
    remarks = concatMap (remark step.before step.model name) step.remarks

-- | Returns a remark's lines, rendered against the model before the edit or
-- after it, whichever the remark is about.
remark :: Model -> Model -> Text -> Remark -> [Text]
remark before _ name (AlreadyDecided o v cores) =
  because before name (optionName o <> " was already " <> verdictText v) v o shown
    ++ ["and " <> tshow (length rest) <> " more: quandary why " <> optionName o | not (null rest)]
  where
    (shown, rest) = splitAt 1 cores
remark _ after name (StillDecided o v cores) = because after name (optionName o <> " is still " <> verdictText v) v o cores
remark _ _ _ (ReasonChanged old new) = ["reason: " <> reasonOrNone old <> " -> " <> reasonOrNone new]
remark _ _ _ (AlsoRuledIn p) = ["also ruled so in " <> p]
remark _ _ _ (CommitsToRaiser r) = ["commits to raiser " <> optionName r <> ", which nothing else rules"]
remark _ _ _ (ImpossibleHere o) = [optionName o <> " is impossible at this point"]

reasonOrNone :: Maybe Reason -> Text
reasonOrNone = maybe "none" reasonText

badRank :: Text -> Text
badRank t = "rank " <> t <> ": expected an integer from 0 to " <> tshow maxRank

pointError :: PointError -> Text
pointError (PointExists p) = "point " <> p <> " exists"
pointError (NoSuchPoint p) = "unknown point " <> p
pointError RemovesCurrent = "current cannot be removed"
pointError (ExtendedBy child p) = "point " <> child <> " extends " <> p
