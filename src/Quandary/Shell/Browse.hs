module Quandary.Shell.Browse
  ( Route (..)
  , Page (..)
  , Revision (..)
  , Drawn (..)
  , Shown (..)
  , Live
  , Tui (..)
  , Pane (..)
  , Focus (..)
  , Overlay (..)
  , Listing (..)
  , routePoint
  , routeTitle
  , withLive
  , currentRevision
  , page
  , awaitChange
  , nextPane
  , pageStanding
  , pageQuestion
  , shownQuestion
  , shownOption
  , pageRow
  , pageOptions
  , Target (..)
  , targets
  , targetRoute
  , goToMatches
  , listedQuestions
  , listedIds
  , landing
  , reopenable
  , pointNames
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forM_, void, when)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import Quandary.Core.Analyse
import Quandary.Core.Rulings (EditError)
import Quandary.Core.Rulings qualified as Rulings
import Quandary.Core.Source (Document (..))
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Core.View
import Quandary.Core.View qualified as View
import Quandary.Shell.Fuzzy qualified as Fuzzy
import Quandary.Shell.Resolve (Verb)
import Quandary.Shell.Resolve qualified as Resolve
import Quandary.Shell.Solver.Cadical (cadical)
import Quandary.Shell.Storage.Files (FileProblem (..))
import Quandary.Shell.Storage.Files qualified as Files
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FSNotify (Event (..), EventIsDirectory (..), WatchManager, watchDir, watchTree, withManager)
import System.FilePath (makeRelative, normalise, takeFileName, (</>))

data Route
  = StatusAt Text
  | QuestionAt Text Id
  | OptionAt Text Text
  | Points
  | WhatifAt Text [(Verb, Text)]
  | GoTo Text Text
  deriving (Eq, Ord, Show)

-- | Returns the point the route is at, `current` for the points.
routePoint :: Route -> Text
routePoint (StatusAt p) = p
routePoint (QuestionAt p _) = p
routePoint (OptionAt p _) = p
routePoint Points = Rulings.current
routePoint (WhatifAt p _) = p
routePoint (GoTo p _) = p

routeTitle :: Route -> Text
routeTitle (StatusAt p) = "point " <> p
routeTitle (QuestionAt p q) = idText q <> " at " <> p
routeTitle (OptionAt p o) = o <> " at " <> p
routeTitle Points = "points"
routeTitle (WhatifAt p _) = "whatif at " <> p
routeTitle (GoTo p t) = "go to " <> t <> " at " <> p

data Page
  = StatusPage Text Analysis (Map OptionId Bounds) (Set Text)
  | QuestionPage QuestionView
  | OptionPage OptionView
  | PointsPage [PointView]
  | WhatifPage Text [Supposition] (Either EditError Whatif)
  | -- | The page the route names.
    Redirect Route
  | NotFound Text

newtype Revision = Revision Int
  deriving (Eq, Ord, Show)

data Usability = Unusable [FileProblem] | Usable Model

data Drawn = Drawn
  { revision :: Revision
  , shown :: Shown
  }

data Shown = Problems [FileProblem] | Shown Model Page

data Live = Live
  { dir :: FilePath
  , loaded :: TVar (Revision, Usability)
  , memo :: TVar (Revision, Memo)
  }

-- | What the solver found at one revision.
data Memo = Memo
  { pages :: Map Route Page
  , analyses :: Map Text Analysis
  , staleNames :: Maybe (Set Text)
  }

emptyMemo :: Memo
emptyMemo = Memo Map.empty Map.empty Nothing

-- | Run the continuation on the space, reread whenever its files change.
withLive :: FilePath -> (Live -> IO a) -> IO a
withLive dir k = do
  root <- makeAbsolute dir
  events <- newTVarIO 0
  withManager $ \manager -> do
    watchSpace manager root (atomically (modifyTVar' events (+ 1)))
    first <- Files.readSpace dir
    live <- Live dir <$> newTVarIO (Revision 0, usability dir first) <*> newTVarIO (Revision 0, emptyMemo)
    either absurd id <$> race (settle live events 0 first) (k live)

-- | Run the action on every event touching `quandary.yaml` or the
-- directories of Markdown files.
watchSpace :: WatchManager -> FilePath -> IO () -> IO ()
watchSpace manager root changed = do
  _ <- watchDir manager root atRoot onRootEvent
  forM_ Files.directories $ \d -> do
    exists <- doesDirectoryExist (root </> d)
    when exists (watchUnder d)
  where
    atRoot e = takeFileName e.eventPath `elem` Files.pathOf "" ConfigDoc : Files.directories
    watchUnder d = void (watchTree manager (root </> d) (const True) (const changed))
    onRootEvent e = do
      case e of
        Added {eventIsDirectory = IsDirectory} -> watchUnder (takeFileName e.eventPath)
        _ -> pure ()
      changed

-- | Wait for an event past the count seen, then for 100ms without one,
-- then reread the space, publishing it when it differs from the one read
-- before.
settle :: Live -> TVar Int -> Int -> Files.Loaded -> IO Void
settle live events seen previous = do
  n <- atomically (eventPast events seen) >>= quiet
  reread <- try @IOException (Files.readSpace live.dir)
  next <- case reread of
    Right l | l /= previous -> l <$ publish (usability live.dir l)
    _ -> pure previous
  settle live events n next
  where
    quiet n = do
      threadDelay 100000
      n' <- readTVarIO events
      if n' /= n then quiet n' else pure n
    publish l = atomically $ do
      (Revision r, _) <- readTVar live.loaded
      writeTVar live.loaded (Revision (r + 1), l)
      writeTVar live.memo (Revision (r + 1), emptyMemo)

eventPast :: TVar Int -> Int -> STM Int
eventPast events seen = do
  n <- readTVar events
  n <$ check (n /= seen)

-- | Returns the model, or every problem with its file relative to the
-- space.
usability :: FilePath -> Files.Loaded -> Usability
usability dir l = maybe (Unusable problems) Usable (Files.usableModel l v)
  where
    v = validate l.sources
    problems = [p {file = makeRelative (normalise dir) p.file} | p <- Files.problemsOf dir l v]

currentRevision :: Live -> IO Revision
currentRevision live = fst <$> readTVarIO live.loaded

-- | Returns the route's page at the current revision.
page :: Live -> Route -> IO Drawn
page live r = do
  (rev, u) <- readTVarIO live.loaded
  Drawn rev <$> case u of
    Unusable ps -> pure (Problems ps)
    Usable m -> Shown m <$> memoised live rev (Map.lookup r . (.pages)) store (build (Building live rev m) r)
  where
    store pg memo
      | memoisable r pg = memo {pages = Map.insert r pg memo.pages}
      | otherwise = memo

memoisable :: Route -> Page -> Bool
memoisable _ (NotFound _) = False
memoisable (WhatifAt _ _) _ = False
memoisable _ _ = True

-- | A model being drawn at a revision of the space.
data Building = Building
  { live :: Live
  , revision :: Revision
  , model :: Model
  }

build :: Building -> Route -> IO Page
build b (StatusAt p) = atPoint b.model p $ do
  a <- analysisAt b p
  StatusPage p a <$> View.statusBounds cadical b.model p a <*> staleNames b
build b (QuestionAt p i) = atPoint b.model p $ case Map.lookup i b.model.space.questions of
  Nothing -> pure (NotFound ("unknown question " <> idText i))
  Just q -> do
    a <- analysisAt b p
    QuestionPage <$> View.questionView cadical b.model p a q
build b (OptionAt p t) = atPoint b.model p $ case Resolve.optionWithQuestion b.model.space t of
  Nothing -> pure (NotFound ("unknown option " <> t))
  Just (q, o) -> do
    a <- analysisAt b p
    OptionPage <$> View.optionView cadical b.model p a q o
build b (GoTo p t) = atPoint b.model p . pure $ case targets b.model.space of
  ts | (_, target) : _ <- Fuzzy.ranked t ts -> Redirect (targetRoute p target)
  _ -> NotFound ("no id matches " <> t)
build b Points = PointsPage . View.pointViews b.model <$> staleNames b
build b (WhatifAt p ws) = atPoint b.model p $ case traverse (supposed b.model.space) ws of
  Left t -> pure (NotFound ("unknown id " <> t))
  Right ss -> WhatifPage p ss <$> View.whatifFacts cadical b.model p ss

atPoint :: Model -> Text -> IO Page -> IO Page
atPoint m p k
  | p == Rulings.current || Map.member p m.points = k
  | otherwise = pure (NotFound ("unknown point " <> p))

supposed :: Space -> (Verb, Text) -> Either Text Supposition
supposed space (v, t) = maybe (Left t) Right (Resolve.supposing space v t)

analysisAt :: Building -> Text -> IO Analysis
analysisAt b p =
  memoised
    b.live
    b.revision
    (Map.lookup p . (.analyses))
    (\a memo -> memo {analyses = Map.insert p a memo.analyses})
    (analyse cadical b.model p)

staleNames :: Building -> IO (Set Text)
staleNames b = memoised b.live b.revision (.staleNames) (\ss memo -> memo {staleNames = Just ss}) (everyStale cadical b.model)

-- | Returns what the memo holds at the revision, else computes it and
-- keeps it while the revision lasts.
memoised :: Live -> Revision -> (Memo -> Maybe a) -> (a -> Memo -> Memo) -> IO a -> IO a
memoised live rev recall store compute = do
  (at, memo) <- readTVarIO live.memo
  case recall memo of
    Just a | at == rev -> pure a
    _ -> do
      a <- compute
      atomically (modifyTVar' live.memo (storeAt rev (store a)))
      pure a

storeAt :: Revision -> (Memo -> Memo) -> (Revision, Memo) -> (Revision, Memo)
storeAt rev f (at, memo)
  | at == rev = (at, f memo)
  | otherwise = (at, memo)

-- | Block until the revision differs from the given one, and return it.
awaitChange :: Live -> Revision -> IO Revision
awaitChange live rev = atomically $ do
  (r, _) <- readTVar live.loaded
  r <$ check (r /= rev)

data Tui = Tui
  { live :: Live
  , route :: Route
  , back :: [Route]
  , drawn :: Drawn
  , focus :: Focus
  , overlay :: Maybe Overlay
  , folded :: Bool
  -- ^ Hides closed and deferred questions but the one shown.
  }

data Pane = Questions | Options | Reading
  deriving (Eq, Show)

nextPane :: Pane -> Pane
nextPane Questions = Options
nextPane Options = Reading
nextPane Reading = Questions

data Focus = Focus
  { pane :: Pane
  , option :: Maybe OptionId
  , expanded :: Set OptionId
  }

-- | A picker over the current page: the points, at the index selected;
-- what to suppose of an option; or the ids matching the text typed, at the
-- index selected.
data Overlay = PointPicker Int | WhatifPicker OptionId | GoToPicker Text Int

-- | A status group in the question list: its questions, or how many it
-- holds when folded.
data Listing = Unfolded GroupKind [Id] | Folded GroupKind Int

listedIds :: Listing -> [Id]
listedIds (Unfolded _ ids) = ids
listedIds (Folded _ _) = []

-- | Returns the page's model and standing when it shows a point's
-- questions.
pageStanding :: Drawn -> Maybe (Model, Standing)
pageStanding d = case d.shown of
  Shown m (StatusPage _ a _ _) -> (,) m <$> standing a
  Shown m (QuestionPage v) -> (,) m <$> standing v.analysis
  Shown m (OptionPage v) -> (,) m <$> standing v.analysis
  _ -> Nothing

pageQuestion :: Drawn -> Maybe QuestionView
pageQuestion d = case d.shown of
  Shown _ (QuestionPage v) -> Just v
  _ -> Nothing

-- | Returns the question the page shows, or whose option it shows.
shownQuestion :: Drawn -> Maybe Id
shownQuestion d = case d.shown of
  Shown _ (QuestionPage v) -> Just v.question.id
  Shown _ (OptionPage v) -> Just v.question.id
  _ -> Nothing

shownOption :: Drawn -> Maybe OptionId
shownOption d = case d.shown of
  Shown _ (OptionPage v) -> Just v.row.option
  _ -> Nothing

-- | Returns the row of the option among the shown question's options, or
-- of the option shown.
pageRow :: Drawn -> OptionId -> Maybe OptionRow
pageRow d o = case d.shown of
  Shown _ (QuestionPage v) -> find ((== o) . (.option)) v.rows
  Shown _ (OptionPage v) | v.row.option == o -> Just v.row
  _ -> Nothing

-- | Returns each group's questions, a folded group's only when it holds
-- the shown question.
listedQuestions :: Bool -> Model -> Text -> Standing -> Maybe Id -> [Listing]
listedQuestions folded m point s shown = [listing g | g <- View.statusGroups m point Map.empty s, not (null g.entries)]
  where
    listing g
      | folded && foldable g.kind && all ((/= shown) . Just) ids = Folded g.kind (length ids)
      | otherwise = Unfolded g.kind ids
      where
        ids = [e.question.id | e <- g.entries]

foldable :: GroupKind -> Bool
foldable Frontier = False
foldable TakenMore = False
foldable RaisedIfTaken = False
foldable ClosedGroup = True
foldable DeferredGroup = True
foldable DormantGroup = False

-- | Returns the route to the first question a fresh status page lists, in
-- place of the page.
landing :: Bool -> Drawn -> Maybe Route
landing folded d = case d.shown of
  Shown m (StatusPage p a _ _) -> do
    s <- standing a
    QuestionAt p <$> listToMaybe (concatMap listedIds (listedQuestions folded m p s Nothing))
  _ -> Nothing

-- | Returns the options the page lists in the order drawn: a question's
-- options, each expanded one followed by the options of the questions it
-- raises; an option's raised questions' options, likewise.
pageOptions :: Set OptionId -> Drawn -> [OptionId]
pageOptions expanded d = case d.shown of
  Shown _ (QuestionPage v) -> concat [r.option : below r.option r.objections | r <- v.rows]
  Shown _ (OptionPage v) -> raised v.row.objections
  _ -> []
  where
    raised obs = concat [c.option : below c.option c.objections | ob <- obs, c <- ob.options]
    below o obs
      | Set.member o expanded = raised obs
      | otherwise = []

-- | What going to an id opens.
data Target = ToQuestion Id | ToOption OptionId
  deriving (Eq, Show)

-- | Returns every question and option id, as written, with what it opens.
targets :: Space -> [(Text, Target)]
targets space =
  concat
    [ (idText q.id, ToQuestion q.id) : [(optionName o, ToOption o) | o <- choices q]
    | q <- Map.elems space.questions
    ]

-- | Returns the ids of the shown space the text matches, best first.
goToMatches :: Text -> Drawn -> [(Text, Target)]
goToMatches t d = case d.shown of
  Shown m _ -> Fuzzy.ranked t (targets m.space)
  Problems _ -> []

targetRoute :: Text -> Target -> Route
targetRoute p (ToQuestion q) = QuestionAt p q
targetRoute p (ToOption o) = OptionAt p (optionName o)

-- | Holds when the row has a ruling to reopen.
reopenable :: OptionRow -> Bool
reopenable r = isJust r.ruling

pointNames :: Drawn -> [Text]
pointNames d = case d.shown of
  Shown m _ -> Set.toList (Set.insert Rulings.current (Map.keysSet m.points))
  Problems _ -> [Rulings.current]
