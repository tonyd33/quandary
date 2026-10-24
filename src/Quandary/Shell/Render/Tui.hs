module Quandary.Shell.Render.Tui
  ( Name (..)
  , draw
  , attributes
  )
where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (centerLayer)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Quandary.Core.Analyse
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Core.View qualified as View
import Quandary.Shell.Browse (Drawn (..), Focus (..), Listing (..), Overlay (..), Page (..), Pane (..), Shown (..), Tui (..))
import Quandary.Shell.Browse qualified as Browse
import Quandary.Shell.Render.Markdown (codeAttr, headingAttr, markdown)
import Quandary.Shell.Render.Text qualified as Render

data Name = QuestionList | OptionList | BodyView | PointList | WhatifView
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
      Just (m, s) -> hBox [hLimit 32 (questionList t m s), pageWidget t.focus t.drawn]
      Nothing -> pageWidget t.focus t.drawn

header :: Tui -> Widget Name
header t = txt (" quandary  " <> Browse.routeTitle t.route) <+> fill ' ' <+> txt (counts <> " ")
  where
    counts = maybe "" (summary . snd) (Browse.pageStanding t.drawn)
    summary s = Render.tshow (count isOpen s) <> " open, " <> Render.tshow (count (== Dormant) s) <> " dormant"
    count f s = length (filter f (Map.elems s.states))

footer :: Widget Name
footer = withAttr dimAttr (txt " j/k move  tab pane  enter open  space fold  u raiser  z closed  backspace back  p point  w whatif  q quit")

questionList :: Tui -> Model -> Standing -> Widget Name
questionList t m s =
  borderWithLabel (paneLabel t Questions "Questions") . viewport QuestionList Vertical . vBox $
    concatMap listing (Browse.listedQuestions t.folded m (Browse.routePoint t.route) s shown)
  where
    shown = (.question.id) <$> Browse.pageQuestion t.drawn
    listing (Unfolded k ids) = withAttr dimAttr (txt (Render.groupTitle k)) : map entry ids
    listing (Folded k n) = [withAttr dimAttr (txt (Render.groupTitle k <> " (" <> Render.tshow n <> ")"))]
    entry i
      | Just i == shown = visible (withAttr selectedAttr (txt ("> " <> idText i)))
      | otherwise = txt ("  " <> idText i)

paneLabel :: Tui -> Pane -> Text -> Widget Name
paneLabel t p label
  | t.focus.pane == p = withAttr focusedAttr (txt (padded label))
  | otherwise = txt (padded label)

padded :: Text -> Text
padded label = " " <> label <> " "

-- | Returns the page in full, its selected option and expanded branches
-- marked.
pageWidget :: Focus -> Drawn -> Widget Name
pageWidget f d = case d.shown of
  Problems ps -> framed "the space has problems" (padRight Max (vBox (map (txtWrap . Render.problem "") ps)))
  Shown m pg -> case pg of
    StatusPage p a bs ss -> framed ("point " <> p) (scrolled BodyView (statusWidget m p a bs ss))
    QuestionPage v -> questionWidget f m.space v
    PointsPage ps -> framed "points" (padRight Max (pointsWidget ps))
    WhatifPage p _ r -> framed ("whatif at " <> p) (scrolled WhatifView (txt (either id Text.unlines (Render.whatifResult p r))))
    NotFound t -> framed "not found" (padRight Max (txt t))

framed :: Text -> Widget Name -> Widget Name
framed label = borderWithLabel (txt (padded label))

scrolled :: Name -> Widget Name -> Widget Name
scrolled n = viewport n Vertical . padRight Max

statusWidget :: Model -> Text -> Analysis -> Map OptionId Bounds -> Set Text -> Widget Name
statusWidget m p a bs ss = vBox $ case a of
  StaleAt cores -> map txt (Render.staleLines m p cores)
  Fresh s ->
    concat [withAttr dimAttr (txt (Render.groupTitle g.kind)) : map entry g.entries ++ [txt " "] | g <- View.statusGroups m p bs s, not (null g.entries)]
      ++ [withAttr dimAttr (txt "points"), pointsWidget (View.pointViews m ss)]
  where
    entry e = txtWrap ("  " <> idText e.question.id <> maybe "" ("  " <>) (Render.entryDetail m.space e.detail))

pointsWidget :: [PointView] -> Widget Name
pointsWidget ps = vBox [txt ("  " <> pv.point.name <> "  " <> Render.pointNote pv) | pv <- ps]

questionWidget :: Focus -> Space -> QuestionView -> Widget Name
questionWidget f space v =
  vBox
    [ framed (Render.questionTitle v.analysis v.question) . viewport OptionList Vertical . vBox $
        txt (Render.questionShape v.question) : txt " " : concat (zipWith optionEntry v.rows (Render.optionLines space v.point v.rows))
    , hBorder
    , scrolled BodyView (readingBody space f.option v.question)
    ]
  where
    optionEntry r line =
      highlighted (f.option == Just r.option) (txt line)
        : map (padLeft (Pad 2) . txt) (Render.rowNotes space r)
        ++ [padLeft (Pad 2) (objectionsWidget f r.option r.objections) | not (null r.objections)]

-- | Returns the raised questions in full when the option is expanded, else
-- their ids.
objectionsWidget :: Focus -> OptionId -> [Objection] -> Widget Name
objectionsWidget f o obs
  | Set.member o f.expanded = objectionTree f obs
  | otherwise = withAttr dimAttr (txt ("raises " <> Text.intercalate ", " [idText ob.raising.question.id | ob <- obs] <> "  (space)"))

-- | Returns the selected listed option's description, else the question's.
readingBody :: Space -> Maybe OptionId -> Question -> Widget Name
readingBody space (Just (Listed i)) _ | Just (_, o) <- optionAt space i = vBox [withAttr headingAttr (txtWrap (idText o.id <> ": " <> o.title)), txt " ", markdown o.description]
readingBody _ _ q = markdown q.description

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
        txt ("raises " <> Render.raisedTitle ob.raising <> " [" <> Render.stateText ob.state <> "]")
          : [padLeft (Pad 2) (txtWrap t) | Just t <- [Render.coRaiserText ob.raising]]
          ++ concatMap entry ob.options
    entry c =
      padLeft (Pad 2) (highlighted (f.option == Just c.option) (txt (Text.justifyLeft 12 ' ' (Render.verdictText c.verdict) <> optionName c.option)))
        : [padLeft (Pad 4) (objectionTree f c.objections) | Set.member c.option f.expanded, not (null c.objections)]

overlayWidget :: Tui -> Overlay -> Widget Name
overlayWidget t (PointPicker i) =
  centerLayer . framed "point" . hLimit 40 . vLimit 20 . viewport PointList Vertical . vBox $
    [highlighted (n == i) (txt (padded name)) | (n, name) <- zip [0 ..] (Browse.pointNames t.drawn)]
overlayWidget t (WhatifPicker o) =
  centerLayer . framed "whatif" $
    txt (" " <> optionName o <> "\n a accept  d defeat  " <> whatifKeys (any Browse.reopenable (Browse.pageRow t.drawn o)) <> "esc cancel ")

whatifKeys :: Bool -> Text
whatifKeys True = "r reopen  "
whatifKeys False = ""
