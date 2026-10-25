module Quandary.Shell.Command.Browse (run) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forM_, mfilter, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (elemIndex, (!?))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Graphics.Vty qualified as Vty
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View (QuestionView (..))
import Quandary.Shell.Browse
  ( Drawn (..)
  , Focus (..)
  , Live
  , Overlay (..)
  , Page (..)
  , Pane (..)
  , Revision
  , Route (..)
  , Shown (..)
  , Tui (..)
  )
import Quandary.Shell.Browse qualified as Browse
import Quandary.Shell.Render.Tui (Name (..))
import Quandary.Shell.Render.Tui qualified as Render
import Quandary.Shell.Resolve (Verb (..))
import System.Exit (ExitCode (..))

newtype Changed = Changed Revision

run :: FilePath -> Text -> IO ExitCode
run dir point = Browse.withLive dir $ \live -> do
  chan <- newBChan 8
  (r, d) <- arrive live True (StatusAt point)
  let start =
        Tui
          { live
          , route = r
          , back = []
          , drawn = d
          , focus = Focus {pane = Questions, option = Nothing, expanded = Set.empty}
          , overlay = Nothing
          , folded = True
          }
  withAsync (forwardChanges live chan d.revision) $ \_ -> do
    (_, vty) <- customMainWithDefaultVty (Just chan) browser start
    Vty.shutdown vty
  pure ExitSuccess

-- | Send each revision past the one given to the channel.
forwardChanges :: Live -> BChan Changed -> Revision -> IO a
forwardChanges live chan rev = do
  rev' <- Browse.awaitChange live rev
  writeBChan chan (Changed rev')
  forwardChanges live chan rev'

browser :: App Tui Changed Name
browser =
  App
    { appDraw = Render.draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle
    , appStartEvent = pure ()
    , appAttrMap = const Render.attributes
    }

handle :: BrickEvent Name Changed -> EventM Name Tui ()
handle (AppEvent (Changed _)) = refresh
handle (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt
handle (VtyEvent (Vty.EvKey k [])) =
  gets (.overlay) >>= \case
    Just o -> overlayKey o k
    Nothing -> do
      p <- gets (.focus.pane)
      key p k
handle _ = pure ()

key :: Pane -> Vty.Key -> EventM Name Tui ()
key _ (Vty.KChar 'q') = halt
key p (Vty.KChar '\t') = setPane (Browse.nextPane p)
key p (Vty.KChar 'j') = move p 1
key p Vty.KDown = move p 1
key p (Vty.KChar 'k') = move p (-1)
key p Vty.KUp = move p (-1)
key p Vty.KEnter = enter p
key _ (Vty.KChar ' ') = selected >>= mapM_ toggle
key _ Vty.KBS = goBack
key _ (Vty.KChar 'p') = openPointPicker
key _ (Vty.KChar 'w') = selected >>= mapM_ (setOverlay . WhatifPicker)
key _ (Vty.KChar 'z') = modify (\t -> t {folded = not t.folded})
key _ (Vty.KChar 'u') = openRaiser
key _ _ = pure ()

overlayKey :: Overlay -> Vty.Key -> EventM Name Tui ()
overlayKey _ Vty.KEsc = closeOverlay
overlayKey (PointPicker i) (Vty.KChar 'j') = movePointPicker i 1
overlayKey (PointPicker i) Vty.KDown = movePointPicker i 1
overlayKey (PointPicker i) (Vty.KChar 'k') = movePointPicker i (-1)
overlayKey (PointPicker i) Vty.KUp = movePointPicker i (-1)
overlayKey (PointPicker i) Vty.KEnter = do
  names <- gets (Browse.pointNames . (.drawn))
  closeOverlay
  forM_ (names !? i) (open . StatusAt)
overlayKey (WhatifPicker o) (Vty.KChar 'a') = suppose AcceptVerb o
overlayKey (WhatifPicker o) (Vty.KChar 'd') = suppose DefeatVerb o
overlayKey (WhatifPicker o) (Vty.KChar 'r') = do
  yes <- gets (\t -> any Browse.reopenable (Browse.pageRow t.drawn o))
  when yes (suppose ReopenVerb o)
overlayKey _ _ = pure ()

setOverlay :: Overlay -> EventM Name Tui ()
setOverlay o = modify (\t -> t {overlay = Just o})

closeOverlay :: EventM Name Tui ()
closeOverlay = modify (\t -> t {overlay = Nothing})

-- | Open the point picker at the point shown.
openPointPicker :: EventM Name Tui ()
openPointPicker = do
  t <- get
  setOverlay (PointPicker (fromMaybe 0 (elemIndex (Browse.routePoint t.route) (Browse.pointNames t.drawn))))

movePointPicker :: Int -> Int -> EventM Name Tui ()
movePointPicker i d = do
  n <- gets (length . Browse.pointNames . (.drawn))
  setOverlay (PointPicker (max 0 (min (n - 1) (i + d))))

-- | Close the overlay and show what supposing the verb of the option would
-- move.
suppose :: Verb -> OptionId -> EventM Name Tui ()
suppose verb o = do
  closeOverlay
  p <- gets (Browse.routePoint . (.route))
  open (WhatifAt p [(verb, optionName o)])

setPane :: Pane -> EventM Name Tui ()
setPane p = modify (\t -> t {focus = t.focus {pane = p}})

move :: Pane -> Int -> EventM Name Tui ()
move Questions d =
  gets questionIds >>= \case
    Just (shown, ids) -> do
      t <- get
      forM_ (step d shown ids) $ \i -> goTo t.back (QuestionAt (Browse.routePoint t.route) i)
    Nothing -> scrollBody d
move Options d =
  gets (Browse.pageQuestion . (.drawn)) >>= \case
    Just v -> selectOption d v
    Nothing -> scrollBody d
move Reading d = scrollBody d

-- | Returns the question shown and the questions listed, when the page
-- shows a point's questions.
questionIds :: Tui -> Maybe (Maybe Id, [Id])
questionIds t = do
  (m, s) <- Browse.pageStanding t.drawn
  let shown = (.question.id) <$> Browse.pageQuestion t.drawn
  pure (shown, concatMap Browse.listedIds (Browse.listedQuestions t.folded m (Browse.routePoint t.route) s shown))

selectOption :: Int -> QuestionView -> EventM Name Tui ()
selectOption d v = do
  t <- get
  forM_ (step d t.focus.option (Browse.visibleOptions t.focus.expanded v)) $ \o ->
    put t {focus = t.focus {option = Just o}}

scrollBody :: Int -> EventM Name Tui ()
scrollBody d = do
  vScrollBy (viewportScroll BodyView) d
  vScrollBy (viewportScroll WhatifView) d

-- | Returns the item d places from the one given, or the first item when
-- the one given is not listed.
step :: Eq a => Int -> Maybe a -> [a] -> Maybe a
step d from xs = case from >>= (`elemIndex` xs) of
  Nothing -> listToMaybe xs
  Just i -> xs !? (i + d)

enter :: Pane -> EventM Name Tui ()
enter Questions = setPane Options
enter Options = selected >>= mapM_ openOrToggle
enter Reading = pure ()

-- | Open the option's question when it is not the one shown, else expand
-- or collapse the option.
openOrToggle :: OptionId -> EventM Name Tui ()
openOrToggle o =
  gets (\t -> otherQuestion t.drawn o) >>= \case
    Just q -> openQuestion q
    Nothing -> toggle o

-- | Returns the option's question when the page shows another question.
otherQuestion :: Drawn -> OptionId -> Maybe Id
otherQuestion d o = case d.shown of
  Shown m (QuestionPage v) -> mfilter (/= v.question.id) (questionOf m.space o)
  _ -> Nothing

openQuestion :: Id -> EventM Name Tui ()
openQuestion q = do
  p <- gets (Browse.routePoint . (.route))
  open (QuestionAt p q)

openRaiser :: EventM Name Tui ()
openRaiser = gets (firstRaiser . (.drawn)) >>= mapM_ openQuestion

-- | Returns the question of the first option raising the one shown.
firstRaiser :: Drawn -> Maybe Id
firstRaiser d = case d.shown of
  Shown m (QuestionPage v) | (o :| _) : _ <- v.question.raisedBy -> questionOf m.space o
  _ -> Nothing

selected :: EventM Name Tui (Maybe OptionId)
selected = gets (.focus.option)

toggle :: OptionId -> EventM Name Tui ()
toggle o = modify (\t -> t {focus = t.focus {expanded = flipped t.focus.expanded}})
  where
    flipped e
      | Set.member o e = Set.delete o e
      | otherwise = Set.insert o e

open :: Route -> EventM Name Tui ()
open r = gets (\t -> t.route : t.back) >>= (`goTo` r)

goBack :: EventM Name Tui ()
goBack =
  gets (.back) >>= \case
    r : rest -> goTo rest r
    [] -> pure ()

-- | Show the route's page with the history given, nothing selected.
goTo :: [Route] -> Route -> EventM Name Tui ()
goTo back r = do
  t <- get
  (r', d) <- liftIO (arrive t.live t.folded r)
  put t {route = r', back, drawn = d, focus = t.focus {option = Nothing, expanded = Set.empty}}

-- | Returns the route shown for the route and its page: a point's first
-- listed question in place of its status.
arrive :: Live -> Bool -> Route -> IO (Route, Drawn)
arrive live folded r = do
  d <- Browse.page live r
  case Browse.landing folded d of
    Just r' -> (,) r' <$> Browse.page live r'
    Nothing -> pure (r, d)

-- | Redraw the route's page, keeping the selected option and the expanded
-- branches that still show.
refresh :: EventM Name Tui ()
refresh = do
  t <- get
  (r, d) <- liftIO (arrive t.live t.folded t.route)
  let kept = maybe Set.empty (Set.fromList . Browse.visibleOptions t.focus.expanded) (Browse.pageQuestion d)
  put
    t
      { route = r
      , drawn = d
      , focus = t.focus {option = mfilter (`Set.member` kept) t.focus.option, expanded = Set.intersection t.focus.expanded kept}
      }
