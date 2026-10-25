module Quandary.Core.Validate
  ( Model (..)
  , Validation (..)
  , validate
  )
where

import Control.Monad (forM, forM_, join, unless, when)
import Data.Char (isAsciiLower, isDigit, isHexDigit)
import Data.List (find, sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Rulings (current, effective, emptyPoint)
import Quandary.Core.Source
import Quandary.Core.Types

data Model = Model
  { space :: Space
  , points :: Map Text Point
  }
  deriving (Eq, Show)

data Validation = Validation
  { problems :: [Problem]
  , model :: Maybe Model
  -- ^ Nothing when a problem blocks.
  }
  deriving (Eq, Show)

-- | Holds for every issue but a ruling's recorded title gone out of date.
blocks :: Issue -> Bool
blocks TitleMismatch {} = False
blocks _ = True

validate :: Sources -> Validation
validate sources =
  Validation
    { problems
    , model = if any (blocks . (.issue)) problems then Nothing else Just m
    }
  where
    (problems, m) = checkSources sources

type Check = (,) [Problem]

report :: Loc -> Issue -> Check ()
report l i = ([Problem l i], ())

checkSources :: Sources -> Check Model
checkSources sources = do
  checkIds sources
  grades <- checkGrades sources.config
  let names = namesOf sources grades
  questions <- mapM (checkQuestion names) sources.questions
  checkForbidsDistinct sources.questions
  notes <- mapM (checkNote names) sources.notes
  let space =
        Space
          { grades
          , questions = Map.fromList [(q.id, q) | q <- questions]
          , notes = Map.fromList [(n.id, n) | n <- notes]
          }
  points <- checkPoints names space sources.points
  pure Model {space, points}

-- | What references resolve against.
data Names = Names
  { options :: Map Text OptionId
  , questions :: Map Text Id
  , targets :: Map Text Id
  , grades :: [Grade]
  }

namesOf :: Sources -> [Grade] -> Names
namesOf sources grades =
  Names
    { options = Map.fromList (listed ++ others)
    , questions = Map.fromList [(q.id.value, Id q.id.value) | q <- sources.questions]
    , targets =
        Map.fromList
          ( [(t, Id t) | t <- map fst listed ++ map fst others]
              ++ [(q.id.value, Id q.id.value) | q <- sources.questions]
              ++ [(n.id.value, Id n.id.value) | n <- sources.notes]
          )
    , grades
    }
  where
    listed = [(o, Listed (Id o)) | q <- sources.questions, opt <- q.options, let o = optionIdOf q opt]
    others = [(q.id.value <> ".other", Other (Id q.id.value)) | q <- sources.questions]

-- | Returns the option's id: its question's directory and its key.
optionIdOf :: QuestionSource -> OptionSource -> Text
optionIdOf q o = q.scope <> o.key.value

checkIds :: Sources -> Check ()
checkIds sources = do
  forM_ sources.questions $ \q -> do
    checkId q.id
    forM_ q.options $ \o -> do
      checkSegment o.key
      when (Text.toLower o.key.value == "other") (report o.key.at ReservedKey)
  forM_ sources.notes (checkId . (.id))
  forM_ sources.points (checkId . (.name))
  forM_ (sortOn (folded . fst) (repeats folded nodes)) $ \(first, n) ->
    report n.at (Collision n.value first.at)
  where
    folded n = Text.toLower n.value
    nodes =
      [q.id | q <- sources.questions]
        ++ [Located o.key.at (optionIdOf q o) | q <- sources.questions, o <- q.options]
        ++ [n.id | n <- sources.notes]

checkId :: Located Text -> Check ()
checkId l = unless (validId l.value) (report l.at (BadId l.value))

checkSegment :: Located Text -> Check ()
checkSegment l = unless (validSegment l.value) (report l.at (BadId l.value))

checkGrades :: ConfigSource -> Check [Grade]
checkGrades c = case c.grades of
  Nothing -> pure []
  Just (Located l []) -> [] <$ report l (EmptyList GradesField)
  Just (Located _ gs) -> do
    forM_ (repeats (.value) (mapMaybe (.name) gs)) $ \(_, n) -> report n.at (DuplicateGrade n.value)
    catMaybes <$> mapM checkGrade gs

checkGrade :: GradeSource -> Check (Maybe Grade)
checkGrade g = case g.name of
  Nothing -> Nothing <$ report g.at (Missing NameField)
  Just n -> do
    unless (validSegment n.value) (report n.at (BadGrade n.value))
    desc <- join <$> traverse checkDesc g.desc
    color <- join <$> traverse checkColor g.color
    pure (Just Grade {name = n.value, desc, color})

-- | Returns the description, or Nothing after reporting a blank one.
checkDesc :: Located Text -> Check (Maybe Text)
checkDesc t
  | Text.null (Text.strip t.value) = Nothing <$ report t.at EmptyDesc
  | otherwise = pure (Just (Text.strip t.value))

-- | Returns the colour, or Nothing after reporting one that is not `#rgb`
-- or `#rrggbb`.
checkColor :: Located Text -> Check (Maybe Text)
checkColor t
  | isHexColor t.value = pure (Just t.value)
  | otherwise = Nothing <$ report t.at (BadColor t.value)

isHexColor :: Text -> Bool
isHexColor t = case Text.uncons t of
  Just ('#', digits) -> Text.length digits `elem` [3, 6] && Text.all isHexDigit digits
  _ -> False

checkQuestion :: Names -> QuestionSource -> Check Question
checkQuestion names q = do
  title <- titleOf q.id.at q.title
  pick <- case q.pick of
    Nothing -> One <$ report q.id.at (Missing PickField)
    Just (Located _ "one") -> pure One
    Just (Located _ "any") -> pure Any
    Just (Located _ "some") -> pure Some
    Just (Located l p) -> One <$ report l (BadPick p)
  raisedBy <- case q.raisedBy of
    Nothing -> pure []
    Just (Located l []) -> [] <$ report l (EmptyList RaisedByField)
    Just (Located _ rs) -> catMaybes <$> mapM raiser rs
  options <- mapM (checkOption names q) q.options
  forbid <- mapM (checkForbid names) q.forbid
  links <- mapM (checkLink names) q.links
  description <- describe q.id.at q.body.preamble
  pure
    Question
      { id = Id q.id.value
      , title
      , description
      , pick
      , raisedBy
      , options
      , forbid
      , links
      }
  where
    raiser (Located l []) = Nothing <$ report l (EmptyList RaisedByField)
    raiser (Located _ os) = do
      resolved <- mapM (resolveOption names) os
      pure (nonEmpty =<< sequence resolved)

checkOption :: Names -> QuestionSource -> OptionSource -> Check Option
checkOption names q o = do
  title <- titleOf o.key.at o.title
  needs <- references names o.needs
  needsOneOf <- case o.needsOneOf of
    Just (Located l []) -> Nothing <$ report l (EmptyList NeedsOneOfField)
    _ -> nonEmpty <$> references names o.needsOneOf
  excludes <- references names o.excludes
  links <- mapM (checkLink names) o.links
  assessment <- traverse (checkAssessment names) o.assessment
  description <- case [s | s <- q.body.sections, s.level == 2, s.heading.value == oid] of
    [] -> "" <$ report o.key.at (MissingSection oid)
    s : rest -> do
      forM_ rest (\d -> report d.heading.at (DuplicateSection oid))
      describe s.heading.at s.text
  pure
    Option
      { id = Id oid
      , title
      , description
      , needs
      , needsOneOf
      , excludes
      , links
      , assessment
      }
  where
    oid = optionIdOf q o

checkAssessment :: Names -> AssessmentSource -> Check Assessment
checkAssessment names a = do
  grade <- case a.grade of
    Nothing -> Grade "" Nothing Nothing <$ report a.at (Missing GradeField)
    Just g -> declaredGrade names g
  unverified <- flag BadUnverified a.unverified
  pure Assessment {grade, unverified, why = (.value) <$> a.why}

-- | Returns the declared grade with the name, or a placeholder after
-- reporting it undeclared.
declaredGrade :: Names -> Located Text -> Check Grade
declaredGrade names g = case find ((== g.value) . (.name)) names.grades of
  Just declared -> pure declared
  Nothing
    | null names.grades -> Grade g.value Nothing Nothing <$ report g.at NoGrades
    | otherwise -> Grade g.value Nothing Nothing <$ report g.at (UnknownGrade g.value)

checkForbid :: Names -> ForbidSource -> Check Forbid
checkForbid names f = do
  literals <- case f.literals of
    Nothing -> [] <$ report f.at (Missing AllField)
    Just (Located l []) -> [] <$ report l (EmptyList AllField)
    Just (Located _ ls) -> catMaybes <$> mapM (resolveLiteral names) ls
  pure Forbid {literals = Set.fromList literals, why = (.value) <$> f.why}

checkForbidsDistinct :: [QuestionSource] -> Check ()
checkForbidsDistinct qs =
  forM_ (sortOn (literals . fst) (repeats literals forbids)) $ \(first, f) ->
    report f.at (DuplicateForbid first.at)
  where
    forbids = [f | q <- qs, f <- q.forbid, isJust f.literals]
    literals f = Set.fromList . map (.value) . (.value) <$> f.literals

checkLink :: Names -> LinkSource -> Check Link
checkLink names l = do
  rel <- case l.rel of
    Nothing -> "" <$ report l.at (Missing RelField)
    Just r -> r.value <$ unless (label r.value) (report r.at (BadLabel r.value))
  to <- case l.to of
    Nothing -> Id "" <$ report l.at (Missing ToField)
    Just t -> case Map.lookup t.value names.targets of
      Nothing -> Id t.value <$ report t.at (UnknownTarget t.value)
      Just i -> pure i
  pure Link {rel, to, extra = Map.fromList l.extra}
  where
    label t = not (null ws) && all (\w -> not (Text.null w) && Text.all (\c -> isAsciiLower c || isDigit c) w) ws
      where
        ws = Text.splitOn "-" t

checkNote :: Names -> NoteSource -> Check Note
checkNote names n = do
  title <- titleOf n.id.at n.title
  links <- mapM (checkLink names) n.links
  description <- describe n.id.at n.body.preamble
  pure Note {id = Id n.id.value, title, description, links}

checkPoints :: Names -> Space -> [PointSource] -> Check (Map Text Point)
checkPoints names space sources = do
  points <- forM sources $ \p -> do
    forM_ p.extends $ \e ->
      unless (e.value == current || Map.member e.value byName) (report e.at (UnknownPoint e.value))
    proposed <- flag BadProposed p.proposed
    accept <- rulings False p.accept
    defeat <- rulings True p.defeat
    drops <- references names p.drop
    defer <- case p.defer of
      Nothing -> pure []
      Just ds -> fmap catMaybes . forM ds.value $ \d -> case Map.lookup d.value names.questions of
        Nothing -> Nothing <$ report d.at (UnknownQuestion d.value)
        Just i -> pure (Just i)
    prefer <- fmap catMaybes . forM p.prefer $ \(o, n) -> do
      oid <- resolveOption names o
      ranked <- case parseRank n.value of
        Just r -> pure (Just r)
        Nothing -> Nothing <$ report n.at (BadRank n.value)
      pure ((,) <$> oid <*> ranked)
    pure
      Point
        { name = p.name.value
        , extends = (.value) <$> p.extends
        , proposed
        , accept = Map.fromList accept
        , defeat = Map.fromList defeat
        , drop = Set.fromList drops
        , defer = Set.fromList defer
        , prefer = Map.fromList prefer
        , body = p.body
        }
  let byPoint = Map.fromList [(p.name, p) | p <- emptyPoint current : points]
  forM_ sources $ \p -> case chain byPoint p.name.value of
    Left loop -> report (Loc (PointDoc p.name.value) Nothing) (ExtendsCycle loop)
    Right [] -> pure ()
    Right (own : ancestors) -> do
      let here = Loc (PointDoc p.name.value) Nothing
          ruled x = Set.union (Map.keysSet x.accept) (Map.keysSet x.defeat)
          inherited = case ancestors of
            parent : _ -> Map.keysSet (effective byPoint parent.name)
            [] -> Set.empty
      forM_ (Set.toList (Set.intersection (Map.keysSet own.accept) (Map.keysSet own.defeat))) $ \o ->
        report here (AcceptedAndDefeated o)
      forM_ (Set.toList own.drop) $ \o ->
        if Set.member o (ruled own)
          then report here (DropsOwn o)
          else unless (Set.member o inherited) (report here (DropsNothing o))
  pure byPoint
  where
    byName = Map.fromList [(p.name.value, p) | p <- sources]
    listed = optionAt space
    rulings needsReason rs = fmap catMaybes . forM rs $ \r -> do
      oid <- resolveOption names r.option
      forM oid $ \o -> do
        case (o, r.title) of
          (Other _, Just t) -> report t.at (TitleOnOther o)
          (Other _, Nothing) -> pure ()
          (Listed _, Nothing) -> report r.option.at (Missing TitleField)
          (Listed i, Just t) -> forM_ (listed i) $ \(_, now) ->
            when (now.title /= t.value) (report t.at (TitleMismatch o t.value now.title))
        reason <- join <$> traverse checkReason r.reason
        when (needsReason && not (isJust r.reason)) (report r.option.at (Missing ReasonField))
        pure (o, Ruling {title = (.value) <$> r.title, reason})

-- | Returns the reason, or Nothing after reporting a blank one.
checkReason :: Located Text -> Check (Maybe Reason)
checkReason t = maybe (Nothing <$ report t.at EmptyReason) (pure . Just) (parseReason t.value)

-- | Returns the point and the points it extends, nearest first, or the
-- names on a cycle.
chain :: Map Text Point -> Text -> Either [Text] [Point]
chain byPoint = go []
  where
    go seen n
      | n `elem` seen = Left (reverse (n : seen))
      | otherwise = case Map.lookup n byPoint of
          Nothing -> Right []
          Just p -> (p :) <$> maybe (Right []) (go (n : seen)) p.extends

resolveOption :: Names -> Located Text -> Check (Maybe OptionId)
resolveOption names o = case Map.lookup o.value names.options of
  Nothing
    | Map.member o.value names.questions -> Nothing <$ report o.at (QuestionNotOption o.value)
    | otherwise -> Nothing <$ report o.at (UnknownOption o.value)
  Just i -> pure (Just i)

references :: Names -> Maybe (Located [Located Text]) -> Check [OptionId]
references names = maybe (pure []) (fmap catMaybes . mapM (resolveOption names) . (.value))

resolveLiteral :: Names -> Located Text -> Check (Maybe Literal)
resolveLiteral names l = case Text.stripPrefix "not " l.value of
  Just o -> fmap Refuse <$> resolveOption names (Located l.at o)
  Nothing -> fmap Take <$> resolveOption names l

flag :: (Text -> Issue) -> Maybe (Located Text) -> Check Bool
flag _ Nothing = pure False
flag _ (Just (Located _ "true")) = pure True
flag bad (Just (Located l t)) = False <$ report l (bad t)

-- | Returns each element whose key an earlier element holds, paired with
-- the first element holding it, in list order.
repeats :: Ord k => (a -> k) -> [a] -> [(a, a)]
repeats key = go Map.empty
  where
    go _ [] = []
    go firsts (x : xs) = case Map.lookup (key x) firsts of
      Just first -> (first, x) : go firsts xs
      Nothing -> go (Map.insert (key x) x firsts) xs

titleOf :: Loc -> Maybe (Located Text) -> Check Text
titleOf at = maybe ("" <$ report at (Missing TitleField)) (pure . (.value))

describe :: Loc -> Text -> Check Text
describe at t
  | Text.null (Text.strip t) = "" <$ report at MissingDescription
  | otherwise = pure (Text.strip t)
