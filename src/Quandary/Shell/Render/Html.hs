module Quandary.Shell.Render.Html
  ( document
  , titledDocument
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
import Quandary.Core.Compile (Constraint (..))
import Quandary.Core.Compile qualified as Compile
import Quandary.Core.Rulings (EditError, PointEdit (..))
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Shell.Browse
import Quandary.Shell.Markdown qualified as Markdown
import Quandary.Shell.Render.Cytoscape qualified as Cytoscape
import Quandary.Shell.Render.Graph qualified as Graph
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve (Verb (..), verbName)
import Quandary.Shell.Storage.Files (FileProblem)

-- | Returns the route's URL path and query.
link :: Route -> Text
link (StatusAt p) = path ["p", p]
link (QuestionAt p (Id q)) = path (["p", p, "q"] ++ Text.splitOn "/" q)
link (OptionAt p o) = path (["p", p, "o"] ++ Text.splitOn "/" o)
link Points = path ["points"]
link (WhatifAt p ws) = path ["p", p, "whatif"] <> query [(verbName v, t) | (v, t) <- ws]
link (GraphAt p dead) = path ["p", p, "graph"] <> deadQuery dead
link (BoxAt p dead) = path ["p", p, "box"] <> deadQuery dead

deadQuery :: Dead -> Text
deadQuery HideDead = ""
deadQuery ShowDead = "?all"

query :: [(Text, Text)] -> Text
query [] = ""
query qs = utf8 (renderQueryText True [(k, Just v) | (k, v) <- qs])

path :: [Text] -> Text
path = utf8 . encodePathSegments

utf8 :: Builder -> Text
utf8 = decodeUtf8 . Lazy.toStrict . toLazyByteString

-- | Returns the whole page, reloading itself when the space changes.
document :: Route -> Drawn -> Html ()
document r = titledDocument (routePoint r) (routeTitle r)

-- | Returns the whole page at the point under the title, reloading itself
-- when the space changes.
titledDocument :: Text -> Text -> Drawn -> Html ()
titledDocument point title d = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml ("quandary: " <> title))
    link_ [rel_ "stylesheet", href_ "/assets/style.css"]
  body_ $ do
    nav_ $ do
      a_ [href_ (link (StatusAt point))] (toHtml ("point " <> point))
      " "
      a_ [href_ (link (BoxAt point HideDead))] "box"
      " "
      a_ [href_ (link (GraphAt point HideDead))] "graph"
      " "
      a_ [href_ (link Points)] "points"
      " "
      form_ [method_ "get", action_ (path ["p", point, "go"])] $
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
    GraphPage p dead g -> graphHtml p dead g
    BoxPage p dead a -> boxHtml m p dead a
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
      h2_ (toHtml (Render.groupTitle g.kind) >> helpIcon (Render.groupHelp g.kind))
      ul_ [class_ "entries"] $ forM_ g.entries $ \e -> li_ $ do
        questionLink p e.question.id
        span_ [class_ "title"] (toHtml e.question.title)
        detailHtml m.space p e.detail
  p_
    [class_ "muted"]
    (toHtml (Render.tshow (Set.size ss) <> " stale of " <> Render.tshow (Map.size m.points) <> " points"))

-- | Returns what status shows beside a question, each option linked.
detailHtml :: Space -> Text -> Detail -> Html ()
detailHtml space p (Choosing cs) = detailDiv $ do
  unless (null cs.possible) $ span_ [class_ "choices"] (mapM_ (choiceHtml space p) cs.possible)
  unless (null cs.taken) $
    span_ [class_ "taken"] (span_ [class_ "lead", helpOf Render.takenHelp] "taken" >> mapM_ (keyLink p) cs.taken)
detailHtml _ p (RaisedIf rs) = detailDiv (span_ [class_ "lead"] "by" >> chunksHtml p (Render.raisersChunks rs))
detailHtml _ _ (Took []) = pure ()
detailHtml _ p (Took os) = detailDiv (mapM_ (keyLink p) os)
detailHtml _ _ NoDetail = pure ()

detailDiv :: Html () -> Html ()
detailDiv = div_ [class_ "detail"]

choiceHtml :: Space -> Text -> Choice -> Html ()
choiceHtml space p c = span_ [class_ "choice"] (keyLink p c.option >> choiceNote p (Render.choiceNote space c))

questionHtml :: Space -> QuestionView -> Html ()
questionHtml space v = do
  h1_ (toHtml (Render.questionTitle v.analysis v.question))
  p_ [class_ "muted"] $ do
    graphLink v.point (deadness v.analysis v.question.id Nothing) (Cytoscape.questionKey v.question.id)
    chunksHtml v.point (Render.questionShape v.question)
  markdown v.question.description
  table_ [class_ "options"] $ forM_ v.rows $ \r -> do
    let mark = Render.mark r.ruling r.verdict
        notes = Render.rowNotes space r
    tr_ [id_ (anchor r.option)] $ do
      td_
        [class_ "mark"]
        (span_ [class_ ("badge " <> markClass mark), helpOf (Render.markHelp mark)] (toHtml (Render.rowMark r)))
      td_ (optionLink v.point r.option)
      td_ (chunksHtml v.point (Render.rowDetail space v.point r))
      td_ (whatifButtons v.point r)
    unless (null notes) $
      tr_ (td_ "" >> td_ [colspan_ "3", class_ "detail"] (notesHtml v.point notes))
    forM_ r.objections $ \ob ->
      tr_ (td_ "" >> td_ [colspan_ "3"] (objectionHtml v.point ob))
  forM_ v.question.options $ \o -> section_ [id_ ("body-" <> idText o.id)] $ do
    h2_ (optionLink v.point (Listed o.id) >> span_ [class_ "title"] (toHtml o.title))
    markdown o.description

optionHtml :: Model -> OptionView -> Html ()
optionHtml m v = do
  h1_ (toHtml (Render.optionTitle v))
  p_ [class_ "muted"] $ do
    graphLink v.point (deadness v.analysis v.question.id v.row.verdict) (Cytoscape.optionKey v.row.option)
    "in "
    questionLink v.point v.question.id
    span_ [class_ "title"] (toHtml (v.question.title <> Render.stateSuffix v.analysis v.question))
  forM_ v.row.ruling $ \ruling -> p_ (toHtml (Render.rulingSummary v.point ruling))
  causesHtml m v
  unless (null value) $ p_ (chunksHtml v.point value)
  unless (null notes) $ p_ [class_ "detail"] (notesHtml v.point notes)
  div_ (whatifButtons v.point v.row)
  markdown (optionDescription v)
  forM_ v.row.objections (div_ [class_ "raises"] . objectionHtml v.point)
  where
    value = Render.valueChunks m.space v.row
    notes = Render.rowNotes m.space v.row

notesHtml :: Text -> [[Render.Chunk]] -> Html ()
notesHtml p = sequence_ . intersperse (br_ []) . map (chunksHtml p)

whatifButtons :: Text -> OptionRow -> Html ()
whatifButtons p r = form_ [method_ "get", action_ (link (WhatifAt p []))] $
  forM_ ([AcceptVerb, DefeatVerb] ++ [ReopenVerb | reopenable r]) $ \verb ->
    button_ [name_ (verbName verb), value_ (optionName r.option)] (toHtml (verbName verb))

-- | Returns every set of rulings making the option forced or impossible,
-- each with the constraints giving the verdict with it.
causesHtml :: Model -> OptionView -> Html ()
causesHtml m v = do
  forM_ (Render.explained m v) $ \e -> do
    p_ (chunksHtml v.point e.heading)
    unless (null e.rulings) $ ul_ (forM_ e.rulings (li_ . chunksHtml v.point))
    explanationHtml m v e.cause
  unless (null v.causes) $ scripts ["cytoscape.min.js", "cytoscape-dagre.min.js", "graph-style.js", "explain.js"]

-- | Returns the cause's constraints as a graph and as sentences.
explanationHtml :: Model -> OptionView -> Cause -> Html ()
explanationHtml m v c = div_ [class_ "explanation"] $ do
  p_ [class_ "lead"] (span_ [helpOf explanationHelp] "through")
  div_ [class_ "cause-graph"] ""
  ul_ (forM_ c.constraints (li_ . chunksHtml v.point . Render.constraintChunks))
  script_
    [type_ "application/json"]
    ( jsonText
        ( Cytoscape.explanation
            m.space
            (verdictAt v.analysis)
            (link . QuestionAt v.point)
            (link . OptionAt v.point . optionName)
            v.row.option
            c
        )
    )

explanationHelp :: Text
explanationHelp = "A smallest set of the space's rules that, with these rulings, leaves no design giving the option another verdict."

anchor :: OptionId -> Text
anchor o = "option-" <> optionName o

questionLink :: Text -> Id -> Html ()
questionLink p q = a_ [class_ "id question", href_ (link (QuestionAt p q))] (toHtml (idText q))

optionLink :: Text -> OptionId -> Html ()
optionLink p o = optionLinkAs p o (optionName o)

-- | Returns the option's key linked to its page.
keyLink :: Text -> OptionId -> Html ()
keyLink p o = optionLinkAs p o (Render.key o)

optionLinkAs :: Text -> OptionId -> Text -> Html ()
optionLinkAs p o label = a_ [class_ "id option", href_ (link (OptionAt p (optionName o)))] (toHtml label)

-- | Returns a link to the graph centred on the node with the key.
graphLink :: Text -> Dead -> Text -> Html ()
graphLink p dead key = a_ [class_ "tool to-graph", href_ (link (GraphAt p dead) <> "#" <> key)] "graph"

-- | Returns whether a page must show dead parts to show the question, or
-- an option of it with the verdict.
deadness :: Analysis -> Id -> Maybe Verdict -> Dead
deadness a q v
  | hidesOption HideDead v || hidesQuestion HideDead (Map.lookup q . (.states) =<< standing a) = ShowDead
  | otherwise = HideDead

-- | Returns an icon showing what the term means on hover or focus.
helpIcon :: Text -> Html ()
helpIcon t = span_ [class_ "info", tabindex_ "0", helpOf t] "i"

helpOf :: Text -> Attributes
helpOf = data_ "help"

markClass :: Render.Mark -> Text
markClass Render.Accepted = "accepted"
markClass Render.Defeated = "defeated"
markClass (Render.Judged (Just v)) = Render.verdictText v
markClass (Render.Judged Nothing) = "stale"

-- | Returns the raised question folded under its line, its options inside.
objectionHtml :: Text -> Objection -> Html ()
objectionHtml p ob = details_ $ do
  summary_ $ do
    span_ [class_ "lead"] "raises"
    questionLink p ob.raising.question.id
    span_ [class_ "title"] (toHtml ob.raising.question.title)
    span_ [class_ "badge state", helpOf (Render.stateHelp ob.state)] (toHtml (Render.stateText ob.state))
    span_ [class_ "muted"] (toHtml (" " <> Render.tshow (length ob.options) <> " options"))
    coRaisersHtml p ob.raising
  ul_ $ forM_ ob.options $ \c -> li_ $ do
    span_
      [class_ ("badge " <> Render.verdictText c.verdict), helpOf (Render.verdictHelp c.verdict)]
      (toHtml (Render.verdictText c.verdict))
    optionLink p c.option
    choiceNote p (Render.rankChunks c.rank)

coRaisersHtml :: Text -> Raising -> Html ()
coRaisersHtml p r = forM_ (Render.coRaisers r) $ \(lead, cs) ->
  span_ [class_ "detail"] (toHtml lead >> commaSeparated (map (chunksHtml p) cs))

choiceNote :: Text -> [Render.Chunk] -> Html ()
choiceNote _ [] = pure ()
choiceNote p cs = span_ [class_ "note", helpOf Render.boundsHelp] (chunksHtml p cs)

commaSeparated :: [Html ()] -> Html ()
commaSeparated = sequence_ . intersperse ", "

-- | Returns the chunks, each option linked to its page.
chunksHtml :: Text -> [Render.Chunk] -> Html ()
chunksHtml p = mapM_ chunk
  where
    chunk (Render.Words t) = toHtml t
    chunk (Render.Named o) = optionLink p o
    chunk (Render.Asked q) = questionLink p q
    chunk (Render.Ranked n) =
      span_ [class_ "rank", style_ ("--rank: " <> Render.tshow (rankValue n))] (toHtml (Render.rankText (Just n)))

pointsHtml :: [PointView] -> Html ()
pointsHtml ps = do
  h1_ "points"
  table_ $ forM_ ps $ \pv -> tr_ $ do
    td_ (a_ [href_ (link (StatusAt pv.point.name))] (toHtml pv.point.name))
    td_ (toHtml (Render.pointNote pv))

whatifHtml :: Text -> [Supposition] -> Either EditError Whatif -> Html ()
whatifHtml p ss r = do
  h1_ (toHtml ("whatif at " <> p))
  ul_ (forM_ (mapMaybe (supposedHtml p) ss) (\(v, h) -> li_ (toHtml (verbName v <> " ") >> h)))
  pre_ (toHtml (either id Text.unlines (Render.whatifResult p r)))

-- | Returns the verb supposing it and its id linked, nothing for an edit
-- whatif does not take.
supposedHtml :: Text -> Supposition -> Maybe (Verb, Html ())
supposedHtml p (Suppose (Take o)) = Just (AcceptVerb, optionLink p o)
supposedHtml p (Suppose (Refuse o)) = Just (DefeatVerb, optionLink p o)
supposedHtml p (Apply (Reopen o)) = Just (ReopenVerb, optionLink p o)
supposedHtml p (Apply (Undefer q)) = Just (ReopenVerb, questionLink p q)
supposedHtml _ (Apply _) = Nothing

graphHtml :: Text -> Dead -> Graph.Graph -> Html ()
graphHtml p dead g = do
  div_ [class_ "graph-tools"] $ do
    h1_ (toHtml ("graph at " <> p))
    button_ [id_ "expand"] "expand all"
    button_ [id_ "collapse"] "collapse all"
    button_ [id_ "fit"] "fit"
    button_ [id_ "constraints", helpOf constraintsHelp] "constraints"
    deadToggle (GraphAt p) dead
  div_ [id_ "graph"] ""
  script_
    [type_ "application/json", id_ "elements"]
    (jsonText (Cytoscape.elements (link . QuestionAt p) (link . OptionAt p . optionName) g))
  scripts ["cytoscape.min.js", "cytoscape-dagre.min.js", "cytoscape-expand-collapse.js", "graph-style.js", "graph.js"]

scripts :: [Text] -> Html ()
scripts = mapM_ (\a -> script_ [src_ ("/assets/" <> a)] ("" :: Text))

constraintsHelp :: Text
constraintsHelp = "Draw every needs, needs-one-of and excludes edge. Hovering an option draws its own."

-- | Returns a link to the page with dead parts shown, or hidden when they
-- are shown.
deadToggle :: (Dead -> Route) -> Dead -> Html ()
deadToggle r HideDead =
  a_ [class_ "tool", href_ (link (r ShowDead)), helpOf "Also show dormant questions and impossible options."] "show dead"
deadToggle r ShowDead = a_ [class_ "tool", href_ (link (r HideDead))] "hide dead"

-- | Returns each status group's questions as rows of their options, an
-- option's constraints and raised questions highlighted on hover.
boxHtml :: Model -> Text -> Dead -> Analysis -> Html ()
boxHtml m p dead a = do
  div_ [class_ "graph-tools"] $ do
    h1_ (toHtml ("box at " <> p))
    deadToggle (BoxAt p) dead
  p_ [class_ "legend muted"] $ do
    "hover an option:"
    forM_ legend (\(cls, t) -> span_ [class_ cls] t)
  case a of
    StaleAt cores -> pre_ (toHtml (Text.unlines (Render.staleLines m p cores)))
    Fresh s -> do
      let attrs = constraintData m.space
      forM_ (boxGroups m p dead s) $ \g -> do
        h2_ (toHtml (Render.groupTitle g.kind) >> helpIcon (Render.groupHelp g.kind))
        div_ [class_ "box"] (mapM_ (boxRowHtml attrs p) g.rows)
      scripts ["box.js"]

legend :: [(Text, Html ())]
legend =
  [ ("self", "it")
  , ("needs", "needs")
  , ("needs-one-of", "needs one of")
  , ("needed-by", "needed by")
  , ("excludes", "excludes")
  , ("raised", "raised")
  ]

boxRowHtml :: Map OptionId [Attributes] -> Text -> BoxRow -> Html ()
boxRowHtml attrs p r = div_ [class_ "box-row", data_ "key" (Cytoscape.questionKey r.question.id)] $ do
  div_ [class_ "box-question"] (questionLink p r.question.id >> span_ [class_ "title"] (toHtml r.question.title))
  div_ [class_ "cells"] (mapM_ (cellHtml attrs p) r.cells)

cellHtml :: Map OptionId [Attributes] -> Text -> Cell -> Html ()
cellHtml attrs p c =
  a_
    ( [ class_ ("cell badge " <> markClass (Render.mark c.ruling (Just c.verdict)))
      , href_ (link (OptionAt p (optionName c.option)))
      , title_ (optionName c.option <> maybe "" (": " <>) c.title)
      , data_ "key" (Cytoscape.optionKey c.option)
      ]
        ++ Map.findWithDefault [] c.option attrs
    )
    (toHtml (Render.key c.option) >> chunksHtml p (Render.rankChunks c.rank))

-- | Returns, for each option, data attributes holding the keys of the
-- options it needs, needs one of and excludes and of the questions it
-- raises.
constraintData :: Space -> Map OptionId [Attributes]
constraintData space = Map.map attributes (Map.fromListWith (Map.unionWith (<>)) (relations ++ raises))
  where
    relations = [(o, keyed name k) | (c, _) <- Compile.constraints space, (o, (name, k)) <- relation c]
    raises =
      [ (r, keyed "raises" (Cytoscape.questionKey q.id))
      | q <- Map.elems space.questions
      , rs <- q.raisedBy
      , r <- toList rs
      ]
    keyed name k = Map.singleton name (Set.singleton k)
    attributes = map (\(name, ks) -> data_ name (Text.unwords (Set.toList ks))) . Map.toList

relation :: Constraint -> [(OptionId, (Text, Text))]
relation (Needs o p) = [(o, ("needs", Cytoscape.optionKey p))]
relation (NeedsOneOf o ps) = [(o, ("needs-one-of", Cytoscape.optionKey p)) | p <- toList ps]
relation (Excludes o p) = [(o, ("excludes", Cytoscape.optionKey p))]
relation _ = []

-- | Returns the JSON safe inside a `<script>`: every `<` escaped.
jsonText :: Aeson.ToJSON a => a -> Text
jsonText = Text.replace "<" "\\u003c" . decodeUtf8 . Lazy.toStrict . Aeson.encode

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
block (Markdown.List Nothing items) = ul_ (listItems items)
block (Markdown.List (Just start) items) = ol_ [start_ (Text.pack (show start)) | start /= 1] (listItems items)
block Markdown.Rule = hr_ []
block (Markdown.RawBlock _) = pure ()
block (Markdown.Table aligns header rows) = table_ $ do
  thead_ (tr_ (zipWithM_ (cell th_) columns header))
  tbody_ (forM_ rows (tr_ . zipWithM_ (cell td_) columns))
  where
    columns = aligns ++ repeat Markdown.AlignDefault
    cell tag a is = tag (alignStyle a) (inlines is)

listItems :: [[Markdown.Block]] -> Html ()
listItems = mapM_ (li_ . mapM_ block)

alignStyle :: Markdown.Alignment -> [Attributes]
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
safeUrl url = not (any (`Text.isPrefixOf` lower) ["javascript:", "vbscript:", "file:"] || unsafeData)
  where
    lower = Text.toLower (Text.strip url)
    unsafeData = "data:" `Text.isPrefixOf` lower && not (any (`Text.isPrefixOf` lower) images)
    images = ["data:image/png", "data:image/gif", "data:image/jpeg", "data:image/webp"]
