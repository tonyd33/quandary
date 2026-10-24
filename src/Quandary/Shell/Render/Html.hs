module Quandary.Shell.Render.Html
  ( document
  , link
  , markdown
  )
where

import CMark qualified as M
import Control.Monad (forM_, unless)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Lucid
import Network.HTTP.Types.URI (encodePathSegments, renderQueryText)
import Quandary.Core.Analyse
import Quandary.Core.Rulings (EditError)
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Shell.Browse
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve (Verb (..), verbName, written)
import Quandary.Shell.Storage.Files (FileProblem)

-- | Returns the route's URL path and query.
link :: Route -> Text
link (StatusAt p) = path ["p", p]
link (QuestionAt p (Id q)) = path (["p", p, "q"] ++ Text.splitOn "/" q)
link Points = path ["points"]
link (WhatifAt p ws) = path ["p", p, "whatif"] <> query ws
  where
    query [] = ""
    query qs = utf8 (renderQueryText True [(verbName v, Just t) | (v, t) <- qs])

path :: [Text] -> Text
path = utf8 . encodePathSegments

utf8 :: Builder -> Text
utf8 = decodeUtf8 . Lazy.toStrict . toLazyByteString

-- | Returns the whole page, reloading itself when the space changes.
document :: Route -> Drawn -> Html ()
document r d = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml ("quandary: " <> routeTitle r))
    style_ css
  body_ $ do
    nav_ $ do
      a_ [href_ (link (StatusAt (routePoint r)))] (toHtml ("point " <> routePoint r))
      " "
      a_ [href_ (link Points)] "points"
    main_ (pageHtml d)
    script_ ("new EventSource(\"/events?since=" <> revisionText d.revision <> "\").onmessage = () => location.reload();")

revisionText :: Revision -> Text
revisionText (Revision n) = Render.tshow n

pageHtml :: Drawn -> Html ()
pageHtml d = case d.shown of
  Problems ps -> problemsHtml ps
  Shown m pg -> case pg of
    StatusPage p a bs ss -> statusHtml m p a bs ss
    QuestionPage v -> questionHtml m.space v
    PointsPage ps -> pointsHtml ps
    WhatifPage p ss r -> whatifHtml p ss r
    NotFound t -> h1_ "not found" >> p_ (toHtml t)

problemsHtml :: [FileProblem] -> Html ()
problemsHtml ps = do
  h1_ "the space has problems"
  ul_ (forM_ ps (li_ . toHtml . Render.problem ""))

statusHtml :: Model -> Text -> Analysis -> Map OptionId Bounds -> Set Text -> Html ()
statusHtml m p a bs ss = do
  h1_ (toHtml ("point " <> p))
  case a of
    StaleAt cores -> pre_ (toHtml (Text.unlines (Render.staleLines m p cores)))
    Fresh s -> forM_ (statusGroups m p bs s) $ \g -> unless (null g.entries) $ do
      h2_ (toHtml (Render.groupTitle g.kind))
      ul_ $ forM_ g.entries $ \e -> li_ $ do
        a_ [href_ (link (QuestionAt p e.question.id))] (toHtml (idText e.question.id))
        toHtml (": " <> e.question.title)
        forM_ (Render.entryDetail m.space e.detail) $ \t -> span_ [class_ "detail"] (toHtml t)
  p_ (toHtml (Render.tshow (Set.size ss) <> " stale of " <> Render.tshow (Map.size m.points) <> " points"))

questionHtml :: Space -> QuestionView -> Html ()
questionHtml space v = do
  h1_ (toHtml (Render.questionTitle v.analysis v.question))
  p_ (toHtml (Render.questionShape v.question))
  unless (null v.question.raisedBy) $ p_ $ do
    "raised by "
    sequence_ (intersperse ", " [raiserLinks g | g <- v.question.raisedBy])
  markdown v.question.description
  table_ [class_ "options"] $ forM_ v.rows $ \r -> do
    tr_ [id_ (anchor r.option)] $ do
      td_ (toHtml (Render.rowMark r))
      td_ (toHtml (optionName r.option))
      td_ (toHtml (Render.rowDetail space v.point r))
      td_ (whatifButtons r)
    forM_ (Render.rowNotes space r) $ \n ->
      tr_ (td_ "" >> td_ [colspan_ "3", class_ "detail"] (toHtml n))
    forM_ r.objections $ \ob ->
      tr_ (td_ "" >> td_ [colspan_ "3"] (objectionHtml v.point ob))
  forM_ v.question.options $ \o -> section_ [id_ ("body-" <> idText o.id)] $ do
    h2_ (toHtml (idText o.id <> ": " <> o.title))
    markdown o.description
  where
    raiserLinks g = sequence_ (intersperse " with " [optionLink space v.point o | o <- toList g])
    whatifButtons r = form_ [method_ "get", action_ (link (WhatifAt v.point []))] $
      forM_ ([AcceptVerb, DefeatVerb] ++ [ReopenVerb | reopenable r]) $ \verb ->
        button_ [name_ (verbName verb), value_ (optionName r.option)] (toHtml (verbName verb))

anchor :: OptionId -> Text
anchor o = "option-" <> optionName o

optionLink :: Space -> Text -> OptionId -> Html ()
optionLink space p o = case questionOf space o of
  Just q -> a_ [href_ (link (QuestionAt p q) <> "#" <> anchor o)] (toHtml (optionName o))
  Nothing -> toHtml (optionName o)

objectionHtml :: Text -> Objection -> Html ()
objectionHtml p ob = do
  "raises "
  a_ [href_ (link (QuestionAt p ob.raising.question.id))] (toHtml (idText ob.raising.question.id))
  toHtml (": " <> ob.raising.question.title <> " [" <> Render.stateText ob.state <> "]")
  forM_ (Render.coRaiserText ob.raising) $ \t -> span_ [class_ "detail"] (toHtml (" " <> t))
  ul_ $ forM_ ob.options $ \c ->
    li_ (toHtml (Render.verdictText c.verdict <> " ") >> toHtml (optionName c.option))

pointsHtml :: [PointView] -> Html ()
pointsHtml ps = do
  h1_ "points"
  table_ $ forM_ ps $ \pv -> tr_ $ do
    td_ (a_ [href_ (link (StatusAt pv.point.name))] (toHtml pv.point.name))
    td_ (toHtml (Render.pointNote pv))

whatifHtml :: Text -> [Supposition] -> Either EditError Whatif -> Html ()
whatifHtml p ss r = do
  h1_ (toHtml ("whatif at " <> p))
  ul_ (forM_ (mapMaybe written ss) (\(v, t) -> li_ (toHtml (verbName v <> " " <> t))))
  pre_ (toHtml (either id Text.unlines (Render.whatifResult p r)))

-- | Returns the Markdown as HTML, dropping raw HTML and unsafe links.
markdown :: Text -> Html ()
markdown t = div_ [class_ "body"] (toHtmlRaw (M.commonmarkToHtml [M.optSafe] t))

css :: Text
css =
  Text.unlines
    [ "body { font-family: system-ui, sans-serif; max-width: 60rem; margin: 1rem auto; padding: 0 1rem; line-height: 1.4; }"
    , "nav { display: flex; gap: 1rem; margin-bottom: 1rem; }"
    , "pre, code { font-family: ui-monospace, monospace; }"
    , "pre { background: #f4f4f4; padding: 0.5rem; overflow-x: auto; }"
    , "table.options td { padding: 0.2rem 0.5rem; vertical-align: top; }"
    , ".detail { color: #555; margin-left: 0.5rem; }"
    , "form { display: inline; }"
    , "section { border-top: 1px solid #ddd; margin-top: 1rem; }"
    ]
