module Quandary.Shell.Render.Html
  ( document
  , link
  , markdown
  )
where

import Control.Monad (forM_, unless, zipWithM_)
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Lucid
import Lucid.Base (makeAttributes)
import Network.HTTP.Types.URI (encodePathSegments, renderQueryText)
import Quandary.Core.Analyse
import Quandary.Core.Rulings (EditError, PointEdit (..))
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Shell.Browse
import Quandary.Shell.Markdown qualified as Markdown
import Quandary.Shell.Render.Cytoscape qualified as Cytoscape
import Quandary.Shell.Render.Graph qualified as Graph
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve (Verb (..), verbName, written)
import Quandary.Shell.Storage.Files (FileProblem)

-- | Returns the route's URL path and query.
link :: Route -> Text
link (StatusAt p) = path ["p", p]
link (QuestionAt p (Id q)) = path (["p", p, "q"] ++ Text.splitOn "/" q)
link (OptionAt p o) = path (["p", p, "o"] ++ Text.splitOn "/" o)
link Points = path ["points"]
link (WhatifAt p ws) = path ["p", p, "whatif"] <> query [(verbName v, t) | (v, t) <- ws]
link (GoTo p t) = path ["p", p, "go"] <> query [("id", t)]
link (GraphAt p) = path ["p", p, "graph"]

query :: [(Text, Text)] -> Text
query [] = ""
query qs = utf8 (renderQueryText True [(k, Just v) | (k, v) <- qs])

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
      a_ [href_ (link (GraphAt (routePoint r)))] "graph"
      " "
      a_ [href_ (link Points)] "points"
      " "
      form_ [method_ "get", action_ (path ["p", routePoint r, "go"])] $
        input_ [type_ "search", name_ "id", placeholder_ "go to id"]
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
    OptionPage v -> optionHtml m v
    PointsPage ps -> pointsHtml ps
    WhatifPage p ss r -> whatifHtml p ss r
    GraphPage p g -> graphHtml p g
    Redirect r -> p_ (a_ [href_ (link r)] (toHtml (link r)))
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
      h2_ (toHtml (Render.groupTitle g.kind) >> info (groupHelp g.kind))
      ul_ [class_ "entries"] $ forM_ g.entries $ \e -> li_ $ do
        questionLink p e.question.id
        span_ [class_ "title"] (toHtml e.question.title)
        detailHtml m.space p e.detail
  p_
    [class_ "muted"]
    (toHtml (Render.tshow (Set.size ss) <> " stale of " <> Render.tshow (Map.size m.points) <> " points"))

-- | Returns what status shows beside a question, each option linked.
detailHtml :: Space -> Text -> Detail -> Html ()
detailHtml space p = \case
  Choosing cs -> detail $ do
    unless (null cs.possible) $ span_ [class_ "choices"] (mapM_ choice cs.possible)
    unless (null cs.taken) $
      span_ [class_ "taken"] (span_ [class_ "lead", helpOf takenHelp] "taken" >> mapM_ (keyLink p) cs.taken)
  RaisedIf rs -> detail (span_ [class_ "lead"] "by" >> commaSeparated (map (raiserHtml p) rs))
  Took [] -> pure ()
  Took os -> detail (mapM_ (keyLink p) os)
  NoDetail -> pure ()
  where
    detail = div_ [class_ "detail"]
    choice c = span_ [class_ "choice"] (keyLink p c.option >> choiceNote (Render.choiceNote space c))

choiceNote :: Text -> Html ()
choiceNote "" = pure ()
choiceNote t = span_ [class_ "note", helpOf boundsHelp] (toHtml t)

commaSeparated :: [Html ()] -> Html ()
commaSeparated = sequence_ . intersperse ", "

raiserHtml :: Text -> NonEmpty OptionId -> Html ()
raiserHtml p g = sequence_ (intersperse " with " (map (optionLink p) (toList g)))

questionHtml :: Space -> QuestionView -> Html ()
questionHtml space v = do
  h1_ (toHtml (Render.questionTitle v.analysis v.question))
  p_ [class_ "muted"] $ do
    graphLink v.point (Cytoscape.questionKey v.question.id)
    toHtml ("pick: " <> Render.pickText v.question.pick)
    case v.question.raisedBy of
      [] -> ", a root"
      rs -> ", raised by " >> commaSeparated (map (raiserHtml v.point) rs)
  markdown v.question.description
  table_ [class_ "options"] $ forM_ v.rows $ \r -> do
    tr_ [id_ (anchor r.option)] $ do
      td_ [class_ "mark"] (span_ [class_ ("badge " <> markClass r), helpOf (markHelp r)] (toHtml (Render.rowMark r)))
      td_ (optionLink v.point r.option)
      td_ (toHtml (Render.rowDetail space v.point r))
      td_ (whatifButtons v.point r)
    unless (null (Render.rowNotes space r)) $
      tr_ (td_ "" >> td_ [colspan_ "3", class_ "detail"] (notesHtml space v.point r))
    forM_ r.objections $ \ob ->
      tr_ (td_ "" >> td_ [colspan_ "3"] (objectionHtml v.point ob))
  forM_ v.question.options $ \o -> section_ [id_ ("body-" <> idText o.id)] $ do
    h2_ (optionLink v.point (Listed o.id) >> span_ [class_ "title"] (toHtml o.title))
    markdown o.description

-- | Returns the option's grade and its dominator, one line each.
notesHtml :: Space -> Text -> OptionRow -> Html ()
notesHtml space p r = sequence_ (intersperse (br_ []) (grade ++ dominator))
  where
    grade = [toHtml (Render.assessmentNote a) | Just a <- [r.assessment]]
    dominator =
      [ do
          toHtml ("dominated: at best " <> Render.gradeName space d.atBest <> ", below ")
          optionLink p d.by
          toHtml (" at worst " <> Render.gradeName space d.byAtWorst)
      | Just d <- [r.dominated]
      ]

whatifButtons :: Text -> OptionRow -> Html ()
whatifButtons p r = form_ [method_ "get", action_ (link (WhatifAt p []))] $
  forM_ ([AcceptVerb, DefeatVerb] ++ [ReopenVerb | reopenable r]) $ \verb ->
    button_ [name_ (verbName verb), value_ (optionName r.option)] (toHtml (verbName verb))

optionHtml :: Model -> OptionView -> Html ()
optionHtml m v = do
  h1_ (toHtml (Render.optionTitle v))
  p_ [class_ "muted"] $ do
    graphLink v.point (Cytoscape.optionKey v.row.option)
    "in "
    questionLink v.point v.question.id
    span_ [class_ "title"] (toHtml (v.question.title <> Render.stateSuffix v.analysis v.question))
  forM_ v.row.ruling $ \ruling -> p_ (toHtml (Render.rulingSummary v.point ruling))
  causesHtml m v
  unless (Text.null (Render.valueText m.space v.row)) $ p_ (toHtml (Render.valueText m.space v.row))
  unless (null (Render.rowNotes m.space v.row)) $ p_ [class_ "detail"] (notesHtml m.space v.point v.row)
  div_ (whatifButtons v.point v.row)
  markdown v.description
  forM_ v.row.objections (div_ [class_ "raises"] . objectionHtml v.point)

-- | Returns every set of rulings making the option forced or impossible.
causesHtml :: Model -> OptionView -> Html ()
causesHtml m v = case v.row.verdict of
  Just verdict
    | any Set.null v.causes ->
        p_ ("no valid design " >> toHtml (Render.designVerb verdict) >> " " >> self >> ", whatever the rulings")
    | otherwise -> forM_ (zip (Render.verdictText verdict <> " because of:" : repeat "and because of:") v.causes) $ \(h, c) -> do
        p_ (toHtml h)
        ul_ (forM_ (Set.toList c) (li_ . rulingHtml))
  Nothing -> pure ()
  where
    self = optionLink v.point v.row.option
    rulingHtml l = do
      toHtml (Render.rulingVerb l <> " ")
      optionLink v.point (optionOf l)
      toHtml (Render.rulingSource m v.point l)

anchor :: OptionId -> Text
anchor o = "option-" <> optionName o

questionLink :: Text -> Id -> Html ()
questionLink p q = a_ [class_ "id question", href_ (link (QuestionAt p q))] (toHtml (idText q))

optionLink :: Text -> OptionId -> Html ()
optionLink p o = a_ [class_ "id option", href_ (link (OptionAt p (optionName o)))] (toHtml (optionName o))

-- | Returns the option's key linked to its page.
keyLink :: Text -> OptionId -> Html ()
keyLink p o = a_ [class_ "id option", href_ (link (OptionAt p (optionName o)))] (toHtml (Render.key o))

-- | Returns a link to the graph centred on the node with the key.
graphLink :: Text -> Text -> Html ()
graphLink p key = a_ [class_ "to-graph", href_ (link (GraphAt p) <> "#" <> key)] "graph"

-- | Returns an icon showing what the term means on hover or focus.
info :: Text -> Html ()
info t = span_ [class_ "info", tabindex_ "0", helpOf t] "i"

helpOf :: Text -> Attributes
helpOf = data_ "help"

groupHelp :: GroupKind -> Text
groupHelp Frontier = "Open questions in the order to decide them, each with the keys of its options still possible."
groupHelp TakenMore = "Open questions with an option taken and more still possible."
groupHelp RaisedIfTaken = "Open questions only an option not yet taken raises, with the options raising them."
groupHelp ClosedGroup = "Questions with one option forced (pick: one), or none possible (pick: any)."
groupHelp DeferredGroup = "Questions this point leaves off the frontier."
groupHelp DormantGroup = "Questions live in no design reaching this point."

verdictHelp :: Verdict -> Text
verdictHelp Forced = "Every design reaching this point takes it."
verdictHelp Possible = "Some design reaching this point takes it, and some does not."
verdictHelp Impossible = "No design reaching this point takes it."

stateHelp :: QuestionState -> Text
stateHelp (Open _) = "Live in some design reaching this point, and neither closed nor deferred."
stateHelp Closed = "One option forced (pick: one), or none possible (pick: any)."
stateHelp Deferred = "This point leaves it off the frontier."
stateHelp Dormant = "Live in no design reaching this point."

markHelp :: OptionRow -> Text
markHelp r = case (r.ruling, r.verdict) of
  (Just (Take _, _, _), _) -> "Accepted by a ruling at this point or one it extends."
  (Just (Refuse _, _, _), _) -> "Defeated by a ruling at this point or one it extends."
  (Nothing, Just v) -> verdictHelp v
  (Nothing, Nothing) -> "The point is stale: no design reaches it."

takenHelp :: Text
takenHelp = "Options every design reaching this point takes."

boundsHelp :: Text
boundsHelp =
  "lo..hi: the best design's value counting every ungraded option at the lowest grade, then at the highest. [N]: the rank prefer gave it, 0 to 10."

-- | Returns the class colouring the row's mark: its verdict, or `ruled`.
markClass :: OptionRow -> Text
markClass r = case (r.ruling, r.verdict) of
  (Just _, _) -> "ruled"
  (Nothing, Just v) -> Render.verdictText v
  (Nothing, Nothing) -> "stale"

-- | Returns the raised question folded under its line, its options inside,
-- each followed by the questions it raises in turn.
objectionHtml :: Text -> Objection -> Html ()
objectionHtml p ob = details_ $ do
  summary_ $ do
    span_ [class_ "lead"] "raises"
    questionLink p ob.raising.question.id
    span_ [class_ "title"] (toHtml ob.raising.question.title)
    span_ [class_ "badge state", helpOf (stateHelp ob.state)] (toHtml (Render.stateText ob.state))
    span_ [class_ "muted"] (toHtml (" " <> Render.tshow (length ob.options) <> " options"))
    coRaisersHtml p ob.raising
  ul_ $ forM_ ob.options $ \c -> li_ $ do
    span_
      [class_ ("badge " <> Render.verdictText c.verdict), helpOf (verdictHelp c.verdict)]
      (toHtml (Render.verdictText c.verdict))
    optionLink p c.option

coRaisersHtml :: Text -> Raising -> Html ()
coRaisersHtml p r = case r.with of
  [] -> pure ()
  [g] -> detail ("with " >> raiserHtml p g)
  gs -> detail ("with any of " >> commaSeparated (map grouped gs))
  where
    detail = span_ [class_ "detail"]
    grouped g@(_ :| []) = raiserHtml p g
    grouped g = "(" >> raiserHtml p g >> ")"

pointsHtml :: [PointView] -> Html ()
pointsHtml ps = do
  h1_ "points"
  table_ $ forM_ ps $ \pv -> tr_ $ do
    td_ (a_ [href_ (link (StatusAt pv.point.name))] (toHtml pv.point.name))
    td_ (toHtml (Render.pointNote pv))

whatifHtml :: Text -> [Supposition] -> Either EditError Whatif -> Html ()
whatifHtml p ss r = do
  h1_ (toHtml ("whatif at " <> p))
  ul_ (forM_ ss (\s -> forM_ (written s) (\(v, _) -> li_ (toHtml (verbName v <> " ") >> supposedLink s))))
  pre_ (toHtml (either id Text.unlines (Render.whatifResult p r)))
  where
    supposedLink (Suppose l) = optionLink p (optionOf l)
    supposedLink (Apply (Reopen o)) = optionLink p o
    supposedLink (Apply (Undefer q)) = questionLink p q
    supposedLink (Apply _) = pure ()

-- | Returns the space as a graph to navigate: questions collapsed to
-- start, a click on an option or a double click on a question opening its
-- page, a click on a question expanding or collapsing it.
graphHtml :: Text -> Graph.Graph -> Html ()
graphHtml p g = do
  div_ [class_ "graph-tools"] $ do
    h1_ (toHtml ("graph at " <> p))
    button_ [id_ "expand"] "expand all"
    button_ [id_ "collapse"] "collapse all"
    button_ [id_ "fit"] "fit"
  div_ [id_ "graph"] ""
  script_
    [type_ "application/json", id_ "elements"]
    (jsonText (Cytoscape.elements (link . QuestionAt p) (link . OptionAt p . optionName) g))
  forM_ ["cytoscape.min.js", "cytoscape-dagre.min.js", "cytoscape-expand-collapse.js"] $ \a ->
    script_ [src_ ("/assets/" <> a)] ("" :: Text)
  script_ graphScript

-- | Returns the JSON with every `<` escaped, so it cannot close the script
-- holding it.
jsonText :: Aeson.ToJSON a => a -> Text
jsonText = Text.replace "<" "\\u003c" . decodeUtf8 . Lazy.toStrict . Aeson.encode

graphScript :: Text
graphScript =
  Text.unlines
    [ "const elements = JSON.parse(document.getElementById('elements').textContent);"
    , "const css = n => getComputedStyle(document.documentElement).getPropertyValue(n).trim();"
    , "const cy = cytoscape({"
    , "  container: document.getElementById('graph'),"
    , "  elements,"
    , "  wheelSensitivity: 0.3,"
    , "  style: ["
    , "    { selector: 'node', style: { label: 'data(label)', color: css('--fg'), 'font-size': 11, 'text-wrap': 'wrap', 'text-max-width': 220, 'text-valign': 'center', shape: 'round-rectangle', width: 'label', height: 'label', padding: 6, 'background-color': css('--bg'), 'border-width': 1, 'border-color': css('--muted') } },"
    , "    { selector: 'node.question', style: { 'text-valign': 'top', 'font-weight': 600, 'background-color': css('--panel'), 'border-color': css('--question') } },"
    , "    { selector: 'node.question.cy-expand-collapse-collapsed-node', style: { 'text-valign': 'center' } },"
    , "    { selector: 'node.forced', style: { 'background-color': css('--forced-bg'), 'border-color': css('--forced') } },"
    , "    { selector: 'node.possible', style: { 'background-color': css('--possible-bg'), 'border-color': css('--possible') } },"
    , "    { selector: 'node.impossible', style: { 'background-color': css('--impossible-bg'), 'border-color': css('--impossible'), color: css('--impossible') } },"
    , "    { selector: 'node.joint', style: { shape: 'ellipse', 'font-size': 9 } },"
    , "    { selector: 'edge', style: { 'curve-style': 'bezier', 'target-arrow-shape': 'triangle', width: 1, 'line-color': css('--muted'), 'target-arrow-color': css('--muted') } },"
    , "    { selector: 'edge.needs-one-of', style: { 'line-style': 'dotted' } },"
    , "    { selector: 'edge.excludes', style: { 'line-style': 'dashed', 'line-color': css('--stale'), 'target-arrow-color': css('--stale'), 'target-arrow-shape': 'tee' } },"
    , "    { selector: ':selected', style: { 'border-width': 3 } }"
    , "  ]"
    , "});"
    , "const layout = { name: 'dagre', rankDir: 'LR', nodeSep: 12, rankSep: 70, animate: false, fit: false };"
    , "const api = cy.expandCollapse({ layoutBy: layout, fisheye: false, animate: false, undoable: false, cueEnabled: false });"
    , "api.collapseAll();"
    , "const readable = () => { cy.fit(undefined, 20); if (cy.zoom() < 0.7) { cy.zoom(0.7); const b = cy.elements().boundingBox(); cy.pan({ x: 20 - b.x1 * 0.7, y: 20 - b.y1 * 0.7 }); } };"
    , "const parentOf = key => { const e = elements.find(e => e.data.id === key); return e && e.data.parent; };"
    , "const show = n => { cy.zoom(1); cy.center(n); cy.$(':selected').unselect(); n.select(); };"
    , "const focusHash = () => {"
    , "  const key = decodeURIComponent(location.hash.slice(1));"
    , "  if (!key) return;"
    , "  const n = cy.getElementById(key);"
    , "  if (n.nonempty()) { show(n); return; }"
    , "  const q = cy.getElementById(parentOf(key) || '');"
    , "  if (q.nonempty() && api.isExpandable(q)) api.expand(q, { layoutBy: { ...layout, stop: () => show(cy.getElementById(key)) } });"
    , "};"
    , "cy.layout({ ...layout, stop: () => { readable(); focusHash(); } }).run();"
    , "window.addEventListener('hashchange', focusHash);"
    , "const open = e => { const h = e.target.data('href'); if (h) location.href = h; };"
    , "cy.on('tap', 'node.option', open);"
    , "cy.on('dbltap', 'node.question', open);"
    , "cy.on('tap', 'node.question', e => { const n = e.target; if (api.isExpandable(n)) api.expand(n); else if (api.isCollapsible(n)) api.collapse(n); });"
    , "document.getElementById('expand').onclick = () => api.expandAll();"
    , "document.getElementById('collapse').onclick = () => api.collapseAll();"
    , "window.addEventListener('resize', () => cy.resize());"
    , "document.getElementById('fit').onclick = () => cy.fit(undefined, 20);"
    ]

-- | Returns the GitHub-flavoured Markdown as HTML, dropping raw HTML and
-- unsafe links.
markdown :: Text -> Html ()
markdown t = div_ [class_ "body"] (mapM_ block (Markdown.parse t))

block :: Markdown.Block -> Html ()
block (Markdown.Paragraph is) = p_ (inlines is)
block (Markdown.Heading n _ is) = tag [] (inlines is)
  where
    tag = [h1_, h2_, h3_, h4_, h5_, h6_] !! (min 6 n - 1)
block (Markdown.CodeBlock t) = pre_ (code_ (toHtml t))
block (Markdown.Quote bs) = blockquote_ (mapM_ block bs)
block (Markdown.List Nothing items) = ul_ (mapM_ (li_ . mapM_ block) items)
block (Markdown.List (Just start) items) = ol_ [makeAttributes "start" (Text.pack (show start)) | start /= 1] (mapM_ (li_ . mapM_ block) items)
block Markdown.Rule = hr_ []
block (Markdown.RawBlock _) = pure ()
block (Markdown.Table aligns header rows) = table_ $ do
  thead_ (tr_ (zipWithM_ (cell th_) columns header))
  tbody_ (forM_ rows (tr_ . zipWithM_ (cell td_) columns))
  where
    columns = aligns ++ repeat Markdown.AlignDefault
    cell tag a is = tag (alignStyle a) (inlines is)
    alignStyle Markdown.AlignDefault = []
    alignStyle Markdown.AlignLeft = [style_ "text-align: left"]
    alignStyle Markdown.AlignCenter = [style_ "text-align: center"]
    alignStyle Markdown.AlignRight = [style_ "text-align: right"]

inlines :: [Markdown.Inline] -> Html ()
inlines = mapM_ inline

inline :: Markdown.Inline -> Html ()
inline (Markdown.Str t) = toHtml t
inline Markdown.SoftBreak = "\n"
inline Markdown.LineBreak = br_ []
inline (Markdown.Code t) = code_ (toHtml t)
inline (Markdown.RawInline _) = pure ()
inline (Markdown.Emph is) = em_ (inlines is)
inline (Markdown.Strong is) = strong_ (inlines is)
inline (Markdown.Strike is) = del_ (inlines is)
inline (Markdown.Link url is)
  | safeUrl url = a_ [href_ url] (inlines is)
  | otherwise = inlines is
inline (Markdown.Image url is)
  | safeUrl url = img_ [src_ url, alt_ (Markdown.plainText is)]
  | otherwise = inlines is

-- | Holds unless the URL's scheme runs script or reads local files:
-- `javascript:`, `vbscript:`, `file:`, and `data:` other than a PNG, GIF,
-- JPEG or WebP image.
safeUrl :: Text -> Bool
safeUrl url = not (any (`Text.isPrefixOf` lower) ["javascript:", "vbscript:", "file:"] || dataUrl)
  where
    lower = Text.toLower (Text.strip url)
    dataUrl = "data:" `Text.isPrefixOf` lower && not (any (`Text.isPrefixOf` lower) images)
    images = ["data:image/png", "data:image/gif", "data:image/jpeg", "data:image/webp"]

css :: Text
css =
  Text.unlines
    [ ":root { color-scheme: light dark; --bg: #fbfbfa; --fg: #1f2328; --muted: #6b7280; --line: #e5e7eb; --panel: #f3f4f6; --accent: #3451b2; --question: #6b3fa0; --forced: #1f7a3a; --forced-bg: #e3f4e6; --possible: #8a6100; --possible-bg: #fdf3d6; --impossible: #6b7280; --impossible-bg: #eceef1; --ruled: #2952a3; --ruled-bg: #e3ebfa; --stale: #b42318; --stale-bg: #fde8e7; }"
    , "@media (prefers-color-scheme: dark) { :root { --bg: #15171a; --fg: #e5e7eb; --muted: #9aa0a6; --line: #2c3036; --panel: #1d2024; --accent: #8fa8ff; --question: #c4a3f0; --forced: #7fd49a; --forced-bg: #17301f; --possible: #e8c165; --possible-bg: #33290f; --impossible: #9aa0a6; --impossible-bg: #24272b; --ruled: #8fb0ff; --ruled-bg: #1b2640; --stale: #ff8a80; --stale-bg: #3a1a18; } }"
    , "* { box-sizing: border-box; }"
    , "body { margin: 0; background: var(--bg); color: var(--fg); font: 15px/1.5 system-ui, -apple-system, \"Segoe UI\", sans-serif; }"
    , "nav { position: sticky; top: 0; z-index: 10; display: flex; align-items: center; gap: 1.25rem; padding: 0.6rem max(1rem, calc((100vw - 62rem) / 2)); background: var(--bg); border-bottom: 1px solid var(--line); }"
    , "nav a { color: var(--fg); text-decoration: none; font-weight: 500; }"
    , "nav a:hover { color: var(--accent); }"
    , "nav form { margin-left: auto; }"
    , "nav input { font: inherit; font-size: 0.9rem; width: 16rem; padding: 0.3rem 0.6rem; border: 1px solid var(--line); border-radius: 6px; background: var(--panel); color: var(--fg); }"
    , "main { max-width: 64rem; margin: 0 auto; padding: 1.5rem 1rem 4rem; }"
    , "main:has(#graph) { max-width: none; padding-bottom: 1rem; }"
    , "h1 { font-size: 1.5rem; line-height: 1.3; margin: 0 0 0.5rem; }"
    , "h2 { font-size: 1.05rem; margin: 2rem 0 0.5rem; padding-bottom: 0.3rem; border-bottom: 1px solid var(--line); }"
    , "a { color: var(--accent); text-decoration: none; }"
    , "a:hover { text-decoration: underline; }"
    , "a.id { font-family: ui-monospace, \"SF Mono\", Menlo, monospace; font-size: 0.86em; white-space: nowrap; }"
    , "a.id.question { color: var(--question); }"
    , "a.id::before { display: inline-block; width: 1.05em; height: 1.05em; margin-right: 0.3em; border-radius: 50%; font-size: 0.75em; font-weight: 700; line-height: 1.05em; text-align: center; vertical-align: 0.1em; color: var(--bg); }"
    , "a.id.question::before { content: \"?\"; background: var(--question); }"
    , "a.id.option::before { content: \"\"; width: 0.6em; height: 0.6em; margin: 0 0.4em 0 0.15em; border-radius: 2px; transform: rotate(45deg); background: var(--accent); vertical-align: 0.15em; }"
    , ".title { margin-left: 0.5rem; }"
    , ".muted, .note, .lead { color: var(--muted); }"
    , ".lead { font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.04em; margin-right: 0.4rem; }"
    , ".note { font-size: 0.85em; margin-left: 0.3rem; }"
    , "ul.entries { list-style: none; padding: 0; margin: 0; }"
    , "ul.entries > li { padding: 0.45rem 0; border-bottom: 1px solid var(--line); }"
    , ".detail { font-size: 0.9rem; margin-top: 0.15rem; color: var(--muted); }"
    , ".detail .id { margin-right: 0.15rem; }"
    , ".choices, .taken { display: inline; }"
    , ".choices + .taken { margin-left: 1rem; }"
    , ".choice { display: inline-block; margin-right: 0.9rem; }"
    , ".taken .id { margin-right: 0.5rem; }"
    , ".badge { display: inline-block; font-size: 0.75rem; font-weight: 500; line-height: 1.5; padding: 0 0.45rem; border-radius: 999px; background: var(--panel); color: var(--muted); margin-right: 0.4rem; white-space: nowrap; }"
    , ".badge.forced { background: var(--forced-bg); color: var(--forced); }"
    , ".badge.possible { background: var(--possible-bg); color: var(--possible); }"
    , ".badge.impossible { background: var(--impossible-bg); color: var(--impossible); }"
    , ".badge.ruled { background: var(--ruled-bg); color: var(--ruled); }"
    , ".badge.stale { background: var(--stale-bg); color: var(--stale); }"
    , ".badge.state { margin-left: 0.4rem; }"
    , "table.options { width: 100%; border-collapse: collapse; margin: 1rem 0; }"
    , "table.options td { padding: 0.45rem 0.5rem; vertical-align: top; border-top: 1px solid var(--line); }"
    , "table.options td.mark { width: 6.5rem; }"
    , "table.options td.detail { border-top: none; padding-top: 0; }"
    , "button { font: inherit; font-size: 0.8rem; padding: 0.1rem 0.5rem; border: 1px solid var(--line); border-radius: 5px; background: var(--panel); color: var(--fg); cursor: pointer; }"
    , "button:hover { border-color: var(--accent); color: var(--accent); }"
    , "form { display: inline-flex; gap: 0.3rem; }"
    , "details { margin: 0.2rem 0; }"
    , "summary { cursor: pointer; }"
    , "details ul { margin: 0.3rem 0 0.4rem; padding-left: 1.4rem; list-style: none; }"
    , "details li { padding: 0.1rem 0; }"
    , "section { margin-top: 1.5rem; }"
    , "pre, code { font-family: ui-monospace, \"SF Mono\", Menlo, monospace; font-size: 0.88em; }"
    , "pre { background: var(--panel); padding: 0.7rem 0.9rem; border-radius: 6px; overflow-x: auto; }"
    , ":not(pre) > code { background: var(--panel); padding: 0.05rem 0.3rem; border-radius: 4px; }"
    , ".body table { border-collapse: collapse; margin: 0.8rem 0; }"
    , ".body td, .body th { border: 1px solid var(--line); padding: 0.3rem 0.6rem; }"
    , ".body th { background: var(--panel); }"
    , ".body blockquote { margin: 0.8rem 0; padding-left: 0.9rem; border-left: 3px solid var(--line); color: var(--muted); }"
    , ".graph-tools { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.6rem; }"
    , ".graph-tools h1 { margin: 0 1rem 0 0; }"
    , ".to-graph { float: right; font-size: 0.85rem; padding: 0.1rem 0.6rem; border: 1px solid var(--line); border-radius: 5px; background: var(--panel); }"
    , "[data-help] { position: relative; cursor: help; }"
    , "[data-help]:hover::after, [data-help]:focus::after { content: attr(data-help); position: absolute; left: 0; top: calc(100% + 0.35rem); z-index: 20; width: max-content; max-width: 22rem; padding: 0.45rem 0.6rem; border-radius: 6px; background: var(--fg); color: var(--bg); font: 400 0.8rem/1.4 system-ui, sans-serif; text-transform: none; letter-spacing: normal; white-space: normal; box-shadow: 0 4px 14px rgb(0 0 0 / 0.18); }"
    , ".info { display: inline-block; width: 1.1em; height: 1.1em; margin-left: 0.4rem; border: 1px solid var(--muted); border-radius: 50%; color: var(--muted); font: italic 600 0.7rem/1.05em Georgia, serif; text-align: center; vertical-align: 0.15em; }"
    , ".info:hover, .info:focus { border-color: var(--accent); color: var(--accent); outline: none; }"
    , "#graph { height: calc(100vh - 8.5rem); border: 1px solid var(--line); border-radius: 8px; background: var(--panel); }"
    ]
