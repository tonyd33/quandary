module Quandary.Shell.Render.Tui
  ( Name (..)
  , draw
  , attributes
  )
where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Lens.Micro ((^.))
import Quandary.Core.Analyse
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Core.View qualified as View
import Quandary.Shell.Browse
  ( Drawn (..)
  , Focus (..)
  , Listing (..)
  , Overlay (..)
  , Page (..)
  , Pane (..)
  , Shown (..)
  , Tui (..)
  )
import Quandary.Shell.Browse qualified as Browse
import Quandary.Shell.Render.Markdown (codeAttr, headingAttr, markdown)
import Quandary.Shell.Render.Text qualified as Render

data Name = QuestionList | OptionList | BodyView | PointList | WhatifView | GoToList
  deriving (Eq, Ord, Show)

selectedAttr, dimAttr, focusedAttr :: AttrName
selectedAttr = attrName "selected"
dimAttr = attrName "dim"
focusedAttr = attrName "focused"

attributes :: AttrMap
attributes =
  attrMap
    Vty.defAttr
    [ (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
    , (dimAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
    , (focusedAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (headingAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (codeAttr, Vty.defAttr `Vty.withForeColor` Vty.cyan)
    ]

draw :: Tui -> [Widget Name]
draw t = maybe [] (\o -> [overlayWidget t o]) t.overlay ++ [vBox [header t, content, footer]]
  where
    content = case Browse.pageStanding t.drawn of
      Just (m, s) -> hBox [questionList t m s, pageWidget t.focus t.drawn]
      Nothing -> pageWidget t.focus t.drawn

header :: Tui -> Widget Name
header t = hBox [txt (" quandary  " <> Browse.routeTitle t.route), padLeft Max (txt (counts <> " "))]
  where
    counts = maybe "" (summary . snd) (Browse.pageStanding t.drawn)
    summary s = Render.tshow (count isOpen s) <> " open, " <> Render.tshow (count (== Dormant) s) <> " dormant"
    count f s = length (filter f (Map.elems s.states))

footer :: Widget Name
footer =
  withAttr
    dimAttr
    ( txtWrap
        " j/k move  tab pane  enter open  space fold  u up  g go to  z closed  backspace back  p point  w whatif  q quit"
    )

-- | Returns the list as wide as its widest line, at most a third of the
-- screen.
questionList :: Tui -> Model -> Standing -> Widget Name
questionList t m s =
  hLimitPercent 33 . hLimit (widest + 2) . paneFrame t.focus Questions "Questions" . viewport QuestionList Vertical . vBox $
    map snd ls
  where
    shown = Browse.shownQuestion t.drawn
    ls = concatMap listing (Browse.listedQuestions t.folded m (Browse.routePoint t.route) s shown)
    widest = maximum (Text.length (padded "Questions") + 2 : map (Text.length . fst) ls)
    listing (Unfolded k ids) = dim (Render.groupTitle k) : map entry ids
    listing (Folded k n) = [dim (Render.groupTitle k <> " (" <> Render.tshow n <> ")")]
    dim l = (l, withAttr dimAttr (clipped l))
    entry i
      | Just i == shown = selected ("> " <> idText i)
      | otherwise = let l = "  " <> idText i in (l, clipped l)
    selected l = (l, visible (withAttr selectedAttr (padRight Max (clipped l))))

-- | Returns the text cut to the width available, an ellipsis marking the
-- cut.
clipped :: Text -> Widget n
clipped l = Widget Fixed Fixed $ do
  w <- (^. availWidthL) <$> getContext
  render . txt $
    if Text.length l > w
      then Text.take (w - 1) l <> "\x2026"
      else l

paneFrame :: Focus -> Pane -> Text -> Widget Name -> Widget Name
paneFrame f p = framedIf (f.pane == p)

-- | Returns the widget boxed under the label, the label bold when the
-- condition holds.
framedIf :: Bool -> Text -> Widget Name -> Widget Name
framedIf True label = borderWithLabel (withAttr focusedAttr (txt (padded label)))
framedIf False label = framed label

padded :: Text -> Text
padded label = " " <> label <> " "

-- | Returns the page in full, its selected option and expanded branches
-- marked.
pageWidget :: Focus -> Drawn -> Widget Name
pageWidget f d = case d.shown of
  Problems ps -> framed "the space has problems" (scrolled BodyView (padLeftRight 1 (wrappedLines (map (Render.problem "") ps))))
  Shown m pg -> case pg of
    StatusPage p a bs ss -> body ("point " <> p) BodyView (statusWidget m p a bs ss)
    QuestionPage v -> questionWidget f m.space v
    OptionPage v -> optionWidget f m v
    Redirect r -> framed "go to" (padRight Max (padLeftRight 1 (txtWrap (Browse.routeTitle r))))
    PointsPage ps -> body "points" BodyView (pointsWidget ps)
    GraphPage _ _ -> framed "graph" (padRight Max (padLeftRight 1 (txtWrap "serve draws the graph")))
    WhatifPage p _ r -> body ("whatif at " <> p) WhatifView (wrappedLines (either pure id (Render.whatifResult p r)))
    NotFound t -> framed "not found" (padRight Max (padLeftRight 1 (txtWrap t)))
  where
    body label n = framedIf (f.pane /= Questions) label . scrolled n . padLeftRight 1

framed :: Text -> Widget Name -> Widget Name
framed label = borderWithLabel (txt (padded label))

scrolled :: Name -> Widget Name -> Widget Name
scrolled n = viewport n Vertical . padRight Max

-- | Returns the lines wrapped, each continuation under the line's first
-- word.
wrappedLines :: [Text] -> Widget n
wrappedLines = vBox . map line . concatMap Text.lines
  where
    line l = case Text.span (== ' ') l of
      (_, "") -> txt " "
      (lead, rest) -> padLeft (Pad (Text.length lead)) (txtWrap rest)

statusWidget :: Model -> Text -> Analysis -> Map OptionId Bounds -> Set Text -> Widget Name
statusWidget m p a bs ss = case a of
  StaleAt cores -> wrappedLines (Render.staleLines m p cores)
  Fresh s ->
    vBox $
      concat
        [ [withAttr dimAttr (txt (Render.groupTitle g.kind)), detailed (map entry g.entries), txt " "]
        | g <- View.statusGroups m p bs s
        , not (null g.entries)
        ]
        ++ [withAttr dimAttr (txt "points"), pointsWidget (View.pointViews m ss)]
  where
    entry e = (idText e.question.id, Render.entryDetail m.space e.detail)

-- | Returns each name with its detail beside it, the details in one column
-- wrapping under themselves, or under the name when the column would be
-- narrower than 30.
detailed :: [(Text, Maybe Text)] -> Widget n
detailed rows = Widget Greedy Fixed $ do
  w <- (^. availWidthL) <$> getContext
  let column = 4 + maximum (0 : map (Text.length . fst) rows)
      row (n, Nothing) = txt ("  " <> n)
      row (n, Just d)
        | w - column >= 30 = txt (Text.justifyLeft column ' ' ("  " <> n)) <+> txtWrap d
        | otherwise = vBox [txt ("  " <> n), padLeft (Pad 6) (txtWrap d)]
  render (vBox (map row rows))

pointsWidget :: [PointView] -> Widget Name
pointsWidget ps = detailed [(pv.point.name, Just (Render.pointNote pv)) | pv <- ps]

questionWidget :: Focus -> Space -> QuestionView -> Widget Name
questionWidget f space v =
  vBox
    [ paneFrame f Options (Render.questionTitle v.analysis v.question)
        . fitted OptionList
        . padRight Max
        . padLeftRight 1
        . vBox
        $ txtWrap (Render.questionShape v.question) : txt " " : concatMap optionEntry v.rows
    , paneFrame f Reading label (scrolled BodyView (padLeftRight 1 reading))
    ]
  where
    (label, reading) = readingBody space f.option v.question
    keyWidth = 2 + maximum (0 : map (Text.length . Render.key . (.option)) v.rows)
    optionEntry r =
      highlighted
        (f.option == Just r.option)
        ( txt (Text.justifyLeft 12 ' ' (Render.rowMark r) <> Text.justifyLeft keyWidth ' ' (Render.key r.option))
            <+> txtWrap (nonEmpty (Render.rowDetail space v.point r))
        )
        : map (padLeft (Pad 12) . txtWrap) (Render.rowNotes space r)
        ++ [padLeft (Pad 12) (objectionsWidget f r.option r.objections) | not (null r.objections)]
    nonEmpty "" = " "
    nonEmpty d = d

optionWidget :: Focus -> Model -> OptionView -> Widget Name
optionWidget f m v =
  vBox
    [ paneFrame f Options (Render.optionTitle v)
        . fitted OptionList
        . padRight Max
        . padLeftRight 1
        . vBox
        $ wrappedLines (("in " <> Render.questionTitle v.analysis v.question) : Render.optionFacts m v)
          : [vBox [txt " ", objectionTree f v.row.objections] | not (null v.row.objections)]
    , paneFrame f Reading "description" (scrolled BodyView (padLeftRight 1 (markdown v.description)))
    ]

-- | Returns the widget in a viewport as tall as its content, at most half
-- the height available.
fitted :: Name -> Widget Name -> Widget Name
fitted n w = Widget Greedy Fixed $ do
  measured <- render (vLimitPercent 50 w)
  render (vLimit (Vty.imageHeight (image measured)) (viewport n Vertical w))

-- | Returns the raised questions in full when the option is expanded, else
-- their ids.
objectionsWidget :: Focus -> OptionId -> [Objection] -> Widget Name
objectionsWidget f o obs
  | Set.member o f.expanded = objectionTree f obs
  | otherwise =
      withAttr
        dimAttr
        (txtWrap ("raises " <> Text.intercalate ", " [idText ob.raising.question.id | ob <- obs] <> "  (space)"))

-- | Returns the label and text of the selected listed option's
-- description, else of the question's.
readingBody :: Space -> Maybe OptionId -> Question -> (Text, Widget Name)
readingBody space (Just (Listed i)) _
  | Just (_, o) <- optionAt space i =
      (idText o.id, vBox [withAttr headingAttr (txtWrap o.title), txt " ", markdown o.description])
readingBody _ _ q = ("description", markdown q.description)

-- | Returns the widget selected and scrolled into view when the condition
-- holds.
highlighted :: Bool -> Widget n -> Widget n
highlighted True w = visible (withAttr selectedAttr w)
highlighted False w = w

-- | Returns the raised questions with their options' verdicts, each option
-- in the expanded set followed by the questions it raises.
objectionTree :: Focus -> [Objection] -> Widget Name
objectionTree f = vBox . map objection
  where
    objection ob =
      vBox $
        txtWrap ("raises " <> Render.raisedTitle ob.raising <> " [" <> Render.stateText ob.state <> "]")
          : [padLeft (Pad 2) (txtWrap t) | Just t <- [Render.coRaiserText ob.raising]]
          ++ concatMap entry ob.options
    entry c =
      padLeft
        (Pad 2)
        ( highlighted
            (f.option == Just c.option)
            (padRight Max (txt (Text.justifyLeft 12 ' ' (Render.verdictText c.verdict) <> optionName c.option)))
        )
        : [padLeft (Pad 4) (objectionTree f c.objections) | Set.member c.option f.expanded, not (null c.objections)]

overlayWidget :: Tui -> Overlay -> Widget Name
overlayWidget t (PointPicker i) =
  centerLayer . framed "point" . hLimit width . vLimit (min 20 (length names)) . viewport PointList Vertical . vBox $
    [highlighted (n == i) (padRight Max (txt (padded name))) | (n, name) <- zip [0 ..] names]
  where
    names = Browse.pointNames t.drawn
    width = min 60 (max 20 (2 + maximum (map Text.length names)))
overlayWidget t (WhatifPicker o) =
  centerLayer . framed "whatif" $
    txt
      ( " "
          <> optionName o
          <> "\n a accept  d defeat  "
          <> whatifKeys (any Browse.reopenable (Browse.pageRow t.drawn o))
          <> "esc cancel "
      )
overlayWidget t (GoToPicker typed i) =
  centerLayer . framed "go to" . hLimit 60 $
    vBox
      [ txt (" > " <> typed)
      , vLimit (min 15 (max 1 (length matches))) . viewport GoToList Vertical . vBox $
          [highlighted (n == i) (padRight Max (txt (padded name))) | (n, (name, _)) <- zip [0 ..] matches]
      , withAttr dimAttr (txt " up/down move  enter open  esc cancel ")
      ]
  where
    matches = Browse.goToMatches typed t.drawn

whatifKeys :: Bool -> Text
whatifKeys True = "r reopen  "
whatifKeys False = ""
