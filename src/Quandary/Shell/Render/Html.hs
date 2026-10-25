module Quandary.Shell.Render.Html
  ( document
  , titledDocument
  , link
  , graphElements
  , markdown
  , matchesHtml
  )
where

import Control.Monad (forM_, unless, when, zipWithM_)
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (toList)
import Data.List (find, intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Lucid
import Lucid.Base (makeAttributes)
import Network.HTTP.Types.URI (encodePathSegments, renderQueryText)
import Quandary.Core.Analyse
import Quandary.Core.Compile qualified as Compile
import Quandary.Core.Next (Lead (..))
import Quandary.Core.Rulings (EditError, PointEdit (..))
import Quandary.Core.Rulings qualified as Rulings
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Core.View
import Quandary.Shell.Assets qualified as Assets
import Quandary.Shell.Browse
import Quandary.Shell.Markdown qualified as Markdown
import Quandary.Shell.Render.Cytoscape qualified as Cytoscape
import Quandary.Shell.Render.Graph qualified as Graph
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Resolve (Verb (..), verbName)
import Quandary.Shell.Search qualified as Search
import Quandary.Shell.Storage.Files (FileProblem, LockState)
import Quandary.Shell.Storage.Files qualified as Files

-- | Returns the route's URL path and query.
link :: Route -> Text
link (StatusAt p) = path ["p", p]
link (QuestionAt p (Id q)) = path (["p", p, "q"] ++ Text.splitOn "/" q)
link (OptionAt p o) = path (["p", p, "o"] ++ Text.splitOn "/" o)
link Points = path ["points"]
link (RulingsAt p) = path ["p", p, "rulings"]
link (SearchAt p t) = path ["p", p, "search"] <> query [("q", t) | not (Text.null t)]
link (WhatifAt p ws) = path ["p", p, "whatif"] <> query [(verbName v, t) | (v, t) <- ws]
link (GraphAt p dead) = path ["p", p, "graph"] <> deadQuery dead
link (BoxAt p dead play) = path ["p", p, "box"] <> boxQuery dead play
link (RecordAt p ws) = path ["p", p, "record"] <> query [(verbName v, t) | (v, t) <- ws]

boxQuery :: Dead -> Maybe [(Verb, Text)] -> Text
boxQuery dead play = case items of
  [] -> ""
  _ -> utf8 (renderQueryText True items)
  where
    items =
      [("play", Nothing) | isJust play]
        ++ [(verbName v, Just t) | (v, t) <- concat play]
        ++ [("all", Nothing) | dead == ShowDead]

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
    link_ [rel_ "stylesheet", href_ (Assets.assetPath "style.css")]
  body_ $ do
    topBar point d
    forM_ (Render.lockNotice d.editLock) (p_ [class_ "banner"] . toHtml)
    div_ [class_ "frame"] $ do
      questionsPane point d
      main_ (pageHtml d)
      inspector d
    palette point
    scripts ["shell.js"]
    unless (answersPost d) $
      script_
        ("new EventSource(\"/events?since=" <> revisionText d.revision <> "\").onmessage = () => " <> changeScript d <> ";")

-- | Returns the bar on every page: the point with the others to switch to,
-- the views, and the box opening the go-to palette.
topBar :: Text -> Drawn -> Html ()
topBar point d = header_ [class_ "top"] $ do
  a_ [class_ "brand", href_ (link (StatusAt point))] "quandary"
  details_ [class_ "points"] $ do
    summary_ (toHtml ("point " <> point))
    ul_ (forM_ (pointNames d) (\n -> li_ (a_ [href_ (link (StatusAt n))] (toHtml n))))
  nav_ (forM_ [StatusView, BoxView, GraphView, RulingsView, PointsView] (viewLink point (viewOf d)))
  a_ [class_ "find", href_ (link (SearchAt point "")), data_ "find" (path ["p", point, "find"])] $ do
    span_ "Go to id or search"
    kbd_ "Ctrl K"

data View = StatusView | BoxView | GraphView | RulingsView | PointsView
  deriving (Eq)

viewName :: View -> Text
viewName StatusView = "status"
viewName BoxView = "box"
viewName GraphView = "graph"
viewName RulingsView = "rulings"
viewName PointsView = "points"

viewRoute :: Text -> View -> Route
viewRoute point StatusView = StatusAt point
viewRoute point BoxView = BoxAt point HideDead Nothing
viewRoute point GraphView = GraphAt point HideDead
viewRoute point RulingsView = RulingsAt point
viewRoute _ PointsView = Points

-- | Returns the view the page shows, if any.
viewOf :: Drawn -> Maybe View
viewOf d = case d.shown of
  Shown _ StatusPage {} -> Just StatusView
  Shown _ BoxPage {} -> Just BoxView
  Shown _ GraphPage {} -> Just GraphView
  Shown _ RulingsPage {} -> Just RulingsView
  Shown _ PointsPage {} -> Just PointsView
  _ -> Nothing

viewLink :: Text -> Maybe View -> View -> Html ()
viewLink point shown v = a_ ([href_ (link (viewRoute point v))] ++ [class_ "on" | shown == Just v]) (toHtml (viewName v))

-- | Returns the point's questions grouped as status groups them, the shown
-- question marked, beside a question or option page.
questionsPane :: Text -> Drawn -> Html ()
questionsPane point d = forM_ ((,) <$> shownQuestion d <*> pageStanding d) $ \(q, (m, s)) ->
  aside_ [class_ "questions", ariaLabel "Questions"] $
    forM_ (statusGroups m point Map.empty s) (paneGroup point q)

paneGroup :: Text -> Id -> Group -> Html ()
paneGroup point shown g = unless (null ids) $
  details_ ([class_ "group"] ++ [opened | choosing g.kind || shown `elem` ids]) $ do
    summary_ (span_ (toHtml (Render.groupTitle g.kind)) >> span_ [class_ "tally"] (toHtml (Render.tshow (length ids))))
    ul_ (forM_ ids (\q -> li_ ([class_ "here" | q == shown]) (questionLink point q)))
  where
    ids = [e.question.id | e <- g.entries]

-- | Returns the panel beside the page: what to grade next on the status
-- page, the selected option on a question page, what to grade next for
-- the option on its page.
inspector :: Drawn -> Html ()
inspector d = case d.shown of
  Shown m (StatusPage p _ _ _) -> forM_ d.leads $ \l ->
    panel (leadHtml m p (Just (Render.leadHeading m.space (Rulings.preferenceAt m.points p) l)) l)
  Shown m (QuestionPage v) -> panel (optionPanels m v d.leads)
  Shown m (OptionPage v) -> forM_ d.leads (panel . leadHtml m v.point Nothing)
  _ -> pure ()
  where
    panel = aside_ [class_ "inspector", ariaLabel "Inspector"]

-- | Returns a panel for each possible option, all but the selected hidden.
optionPanels :: Model -> QuestionView -> Map OptionId Lead -> Html ()
optionPanels m v ls = case filter stillPossible v.rows of
  [] -> p_ [class_ "muted"] "No option is still possible."
  rows -> forM_ rows $ \r ->
    section_ ([class_ "inspect", data_ "option" (optionName r.option)] ++ [hidden_ "" | Just r.option /= selectedIn v]) $
      optionPanel m v.point r (Map.lookup r.option ls)

-- | Returns the option the question page selects first: its first possible
-- one.
selectedIn :: QuestionView -> Maybe OptionId
selectedIn v = (.option) <$> find stillPossible v.rows

-- | Returns the option's facts for the inspector: its id and title, verdict
-- and rank, value, what to grade next, grade, the whatif buttons and a link
-- to its page.
optionPanel :: Model -> Text -> OptionRow -> Maybe Lead -> Html ()
optionPanel m p r lead = do
  p_ [class_ "eyebrow"] "option"
  optionLink p r.option
  h2_ (toHtml (fromMaybe "" r.title))
  p_ [class_ "badges"] $ do
    markBadge r
    chunksHtml p (Render.rankChunks r.rank)
  forM_ r.bounds (valueHtml m.space)
  forM_ lead (leadHtml m p Nothing)
  notesPanel m.space p r
  div_ [class_ "actions"] $ do
    whatifButtons p r
    a_ [href_ (link (OptionAt p (optionName r.option)))] "open option page"

notesPanel :: Space -> Text -> OptionRow -> Html ()
notesPanel space p r = case Render.rowNotes space r of
  [] -> pure ()
  notes -> p_ [class_ "detail"] (notesHtml p notes)

-- | Returns the bounds as a bar over the grades, each grade named below it.
valueHtml :: Space -> Bounds -> Html ()
valueHtml space b = div_ [class_ "value"] $ do
  p_ [class_ "eyebrow"] (span_ [helpOf Render.boundsHelp] "value")
  boundsBar space b
  div_ [class_ "scale"] (forM_ space.grades (span_ . gradeHtml))

-- | Returns the bounds as a bar, then as grades.
valueCell :: Space -> Text -> Bounds -> Html ()
valueCell space p b = do
  boundsBar space b
  span_ [class_ "detail"] (chunksHtml p (Render.boundsChunks space b))

-- | Returns the bounds as a cell per grade: those at the bounds solid,
-- those between them faint, the rest empty.
boundsBar :: Space -> Bounds -> Html ()
boundsBar space b =
  span_ [class_ "bar", role_ "img", ariaLabel described, title_ described] $
    forM_ (zip [0 ..] space.grades) $ \(i, g) ->
      span_ (class_ (cellClass i) : map gradeStyle (maybeToList g.color)) ""
  where
    described = Render.plain (Render.boundsChunks space b)
    cellClass i
      | i < b.lo || i > b.hi = "out"
      | i == b.lo || i == b.hi = "end"
      | otherwise = "in"

-- | Returns the fewest options to grade to move the lead's bound, under the
-- heading when given.
leadHtml :: Model -> Text -> Maybe [Render.Chunk] -> Lead -> Html ()
leadHtml m p heading l = div_ [class_ "lead-box"] $ do
  p_ [class_ "eyebrow"] "grade next"
  forM_ heading (p_ . chunksHtml p)
  p_ (chunksHtml p (Render.leadInstruction m.space l))
  ul_ [class_ "grading"] $ forM_ l.grading $ \o ->
    li_ (optionLink p o >> div_ [class_ "detail"] (toHtml (fromMaybe "" (title o))))
  where
    title = Rulings.titleOf m.space

markBadge :: OptionRow -> Html ()
markBadge r = span_ [class_ ("badge " <> markClass mark), helpOf (Render.markHelp mark)] (toHtml (Render.rowMark r))
  where
    mark = Render.mark r.ruling r.verdict

-- | Returns the go-to palette, opened from the top bar or with Ctrl K.
palette :: Text -> Html ()
palette point = term "dialog" [id_ "palette", ariaLabel "Go to"] $ do
  form_ [method_ "get", action_ (path ["p", point, "go"])] $
    input_ [type_ "search", name_ "id", placeholder_ "Go to id", autocomplete_ "off", ariaLabel "Go to id"]
  ul_ [id_ "palette-results"] ""
  p_ [class_ "keys"] $ do
    "Enter opens, arrows move, Esc closes. "
    a_ [id_ "palette-search", href_ (link (SearchAt point ""))] "Search text and reasons"

-- | Returns the palette's matches, each id linked to its page and followed
-- by its title.
matchesHtml :: Text -> [Match] -> Html ()
matchesHtml p ms = forM_ ms $ \mt -> li_ $ do
  targetLink p mt.target
  span_ [class_ "detail"] (toHtml mt.title)

ariaLabel :: Text -> Attributes
ariaLabel = makeAttributes "aria-label"

opened :: Attributes
opened = makeAttributes "open" ""

-- | Returns the script run when the space changes: the graph updates in
-- place, every other page reloads.
changeScript :: Drawn -> Text
changeScript d = case d.shown of
  Shown _ GraphPage {} -> "updateGraph()"
  _ -> "location.reload()"

-- | Holds for the page answering a posted form.
answersPost :: Drawn -> Bool
answersPost d = case d.shown of
  Shown _ RecordedPage {} -> True
  _ -> False

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
    RulingsPage p rs -> rulingsHtml p rs
    SearchPage p t r -> searchHtml p t r
    WhatifPage p ss r -> whatifHtml p ss r
    GraphPage p dead g -> graphHtml p dead g
    BoxPage box -> boxHtml box
    RecordPage p ws -> recordHtml d.revision d.editLock p ws
    RecordedPage p ws r -> recordedHtml p ws r
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
    Fresh s -> do
      let groups = filter (not . null . (.entries)) (statusGroups m p bs s)
      countsHtml groups
      forM_ groups $ \g -> do
        h2_ (toHtml (Render.groupTitle g.kind) >> helpIcon (Render.groupHelp g.kind))
        groupHtml m.space p g
  p_
    [class_ "muted"]
    (toHtml (Render.tshow (Set.size ss) <> " stale of " <> Render.tshow (Map.size m.points) <> " points"))

-- | Returns how many questions each group holds.
countsHtml :: [Group] -> Html ()
countsHtml groups = div_ [class_ "counts"] $ forM_ groups $ \g ->
  div_ [class_ "count"] (strong_ (toHtml (Render.tshow (length g.entries))) >> span_ (toHtml (Render.groupTitle g.kind)))

-- | Returns a status group: the questions still choosing as cards of their
-- options' bounds and ranks, the rest as lines.
groupHtml :: Space -> Text -> Group -> Html ()
groupHtml space p g
  | choosing g.kind = div_ [class_ "cards"] (mapM_ (questionCard space p) g.entries)
  | otherwise = ul_ [class_ "entries"] (mapM_ (entryHtml space p) g.entries)

entryHtml :: Space -> Text -> Entry -> Html ()
entryHtml space p e = li_ $ do
  questionLink p e.question.id
  span_ [class_ "title"] (toHtml e.question.title)
  detailHtml space p e.detail

questionCard :: Space -> Text -> Entry -> Html ()
questionCard space p e = div_ [class_ "card"] $ do
  questionLink p e.question.id
  p_ [class_ "card-title"] (toHtml e.question.title)
  case e.detail of
    Choosing cs -> do
      ul_ [class_ "card-options"] $ forM_ cs.possible $ \c -> li_ $ do
        keyLink p c.option
        forM_ c.bounds (boundsBar space)
        chunksHtml p (Render.rankChunks c.rank)
      unless (null cs.taken) $
        p_ [class_ "detail"] (span_ [class_ "lead", helpOf Render.takenHelp] "taken" >> mapM_ (keyLink p) cs.taken)
    other -> detailHtml space p other

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
  table_ [class_ "options"] $ do
    thead_ (tr_ (mapM_ th_ ["verdict", "key", "option", "value", "rank", ""]))
    forM_ v.rows $ \r -> do
      optionRow space v.point (selectedIn v) r
      raisesRow v.point r
  forM_ v.question.options $ \o -> section_ [id_ ("body-" <> idText o.id)] $ do
    h2_ (optionLink v.point (Listed o.id) >> span_ [class_ "title"] (toHtml o.title))
    markdown o.description

-- | Returns the option's row: verdict, key, title and ruling, value, rank
-- and whatif buttons; a possible option's row selects its panel.
optionRow :: Space -> Text -> Maybe OptionId -> OptionRow -> Html ()
optionRow space p selected r =
  tr_
    ([id_ (anchor r.option), class_ rowClass, data_ "option" (optionName r.option)] ++ [tabindex_ "0" | stillPossible r])
    $ do
      td_ [class_ "mark"] (markBadge r)
      td_ (keyLink p r.option)
      td_ $ do
        span_ [class_ "option-title"] (toHtml (fromMaybe "" r.title))
        forM_ r.ruling (span_ [class_ "detail"] . toHtml . Render.rulingNote p)
      td_ [class_ "value"] (forM_ r.bounds (valueCell space p))
      td_ [class_ "rank-cell"] (chunksHtml p (Render.rankChunks r.rank))
      td_ (whatifButtons p r)
  where
    rowClass
      | Just r.option == selected = "row selected"
      | otherwise = "row"

-- | Returns the questions the option raises, folded under their count.
raisesRow :: Text -> OptionRow -> Html ()
raisesRow p r = unless (null r.objections) $
  tr_ [class_ "raised-row"] $ do
    td_ ""
    td_ [colspan_ "5"] $ details_ [class_ "raises-all"] $ do
      summary_ (toHtml (raisesCount (length r.objections)))
      mapM_ (objectionHtml p) r.objections

raisesCount :: Int -> Text
raisesCount 1 = "raises 1 question"
raisesCount n = "raises " <> Render.tshow n <> " questions"

optionHtml :: Model -> OptionView -> Html ()
optionHtml m v = do
  h1_ (toHtml (Render.optionTitle v))
  p_ [class_ "muted"] $ do
    graphLink v.point (deadness v.analysis v.question.id v.row.verdict) (Cytoscape.optionKey v.row.option)
    "in "
    questionLink v.point v.question.id
    span_ [class_ "title"] (toHtml (v.question.title <> Render.stateSuffix v.analysis v.question))
  forM_ v.row.ruling $ \ruling -> p_ (toHtml (Render.rulingSummary v.point ruling))
  div_ [class_ "facts"] $ do
    div_ [class_ "fact"] (p_ [class_ "eyebrow"] "verdict" >> markBadge v.row)
    forM_ v.row.bounds $ \b -> div_ [class_ "fact"] (valueHtml m.space b)
    div_ [class_ "fact"] $ do
      p_ [class_ "eyebrow"] "rank and grade"
      chunksHtml v.point (Render.rankChunks v.row.rank)
      notesPanel m.space v.point v.row
  div_ [class_ "actions"] (whatifButtons v.point v.row)
  causesHtml m v
  markdown (optionDescription v)
  forM_ v.row.objections (div_ [class_ "raises"] . objectionHtml v.point)

notesHtml :: Text -> [[Render.Chunk]] -> Html ()
notesHtml p = sequence_ . intersperse (br_ []) . map (chunksHtml p)

whatifButtons :: Text -> OptionRow -> Html ()
whatifButtons p r = verbButtons p ([AcceptVerb, DefeatVerb] ++ [ReopenVerb | reopenable r]) r.option

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
    chunk (Render.Graded g) = gradeHtml g

-- | Returns the grade's name, tinted with its colour and showing its
-- description on hover.
gradeHtml :: Grade -> Html ()
gradeHtml g = case catMaybes [helpOf <$> g.desc, gradeStyle <$> g.color] of
  [] -> toHtml g.name
  attributes -> span_ (class_ ("grade" <> tintedClass g.color) : attributes) (toHtml g.name)

gradeStyle :: Text -> Attributes
gradeStyle c = style_ ("--grade: " <> c)

tintedClass :: Maybe Text -> Text
tintedClass = maybe "" (const " tinted")

pointsHtml :: [PointView] -> Html ()
pointsHtml ps = do
  h1_ "points"
  table_ $ forM_ ps $ \pv -> tr_ $ do
    td_ (a_ [href_ (link (StatusAt pv.point.name))] (toHtml pv.point.name))
    td_ (toHtml (Render.pointNote pv))

rulingsHtml :: Text -> [(Literal, Text, Ruling)] -> Html ()
rulingsHtml p rs = do
  section "accepted" [r | r@(Take _, _, _) <- rs]
  section "defeated" [r | r@(Refuse _, _, _) <- rs]
  where
    section heading [] = h1_ heading >> p_ [class_ "detail"] "none"
    section heading ruled = do
      h1_ heading
      table_ [class_ "options"] $ forM_ ruled $ \r@(l, _, given) -> tr_ $ do
        td_ (optionLink p (optionOf l))
        td_ (toHtml (fromMaybe "" given.title))
        td_ [class_ "detail"] (toHtml (Render.rulingNote p r))
        td_ (verbButtons p [ReopenVerb] (optionOf l))

-- | Returns a whatif button for each verb on the option.
verbButtons :: Text -> [Verb] -> OptionId -> Html ()
verbButtons p verbs o = form_ [method_ "get", action_ (link (WhatifAt p []))] $
  forM_ verbs $
    \verb -> button_ [name_ (verbName verb), value_ (optionName o)] (toHtml (verbName verb))

searchHtml :: Text -> Text -> Search.Results -> Html ()
searchHtml p t r = do
  h1_ (toHtml ("search at " <> p))
  form_ [method_ "get", action_ (path ["p", p, "search"])] $
    input_ [type_ "search", name_ "q", value_ t, placeholder_ "ids, titles, descriptions, reasons", autofocus_]
  unless (Text.null (Text.strip t)) $ do
    hitGroup "ids" (map (targetLink p) r.ids)
    hitGroup "titles" [targetLink p x >> " " >> span_ [class_ "detail"] (toHtml title) | (title, x) <- r.titles]
    hitGroup "text" [targetLink p h.target >> " " >> hitHtml h | h <- r.texts]

hitGroup :: Text -> [Html ()] -> Html ()
hitGroup heading [] = h2_ (toHtml heading) >> p_ [class_ "detail"] "none"
hitGroup heading items = h2_ (toHtml heading) >> ul_ [class_ "hits"] (mapM_ li_ items)

targetLink :: Text -> Search.Target -> Html ()
targetLink p (Search.ToQuestion q) = questionLink p q
targetLink p (Search.ToOption o) = optionLink p o

-- | Returns where the text matched and its line, the match marked.
hitHtml :: Search.Hit -> Html ()
hitHtml h = span_ [class_ "detail"] $ do
  span_ [class_ "lead"] (toHtml (Render.placeName h.place))
  toHtml h.snippet.before
  mark_ (toHtml h.snippet.matched)
  toHtml h.snippet.after

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
    (jsonText (graphElements p g))
  scripts ["cytoscape.min.js", "cytoscape-dagre.min.js", "cytoscape-expand-collapse.js", "graph-style.js", "graph.js"]

-- | Returns the graph's elements, each node linked to its page at the point.
graphElements :: Text -> Graph.Graph -> [Aeson.Value]
graphElements p = Cytoscape.elements (link . QuestionAt p) (link . OptionAt p . optionName)

scripts :: [Text] -> Html ()
scripts = mapM_ (\a -> script_ [src_ (Assets.assetPath a)] ("" :: Text))

constraintsHelp :: Text
constraintsHelp = "Draw every needs, needs-one-of and excludes edge. Hovering an option draws its own."

-- | Returns a link to the page with dead parts shown, or hidden when they
-- are shown.
deadToggle :: (Dead -> Route) -> Dead -> Html ()
deadToggle r HideDead =
  a_ [class_ "tool", href_ (link (r ShowDead)), helpOf "Also show dormant questions and impossible options."] "show dead"
deadToggle r ShowDead = a_ [class_ "tool", href_ (link (r HideDead))] "hide dead"

-- | Returns each status group's questions as rows of their options, an
-- option's constraints and raised questions highlighted on hover. Played,
-- a click on an option cycles its supposition, and what the last one moved
-- is marked.
boxHtml :: Box -> Html ()
boxHtml box = do
  div_ [class_ "graph-tools"] $ do
    h1_ (toHtml (routeTitle (BoxAt box.point box.dead (supposedOf box))))
    deadToggle (\d -> BoxAt box.point d (supposedOf box)) box.dead
    playToggle box
  forM_ box.play (playHtml box)
  p_ [class_ "legend muted"] $ do
    "hover an option:"
    forM_ legend (\(cls, t) -> span_ [class_ cls] t)
  case box.analysis of
    StaleAt cores -> pre_ (toHtml (Text.unlines (Render.staleLines box.model box.point cores)))
    Fresh s -> do
      let context = cellContext box
      forM_ (boxGroups box.model box.point box.dead (standing =<< (.before) =<< box.play) s) $ \g -> do
        h2_ (toHtml (Render.groupTitle g.kind) >> helpIcon (Render.groupHelp g.kind))
        div_ [class_ "box"] (mapM_ (boxRowHtml context) g.rows)
      scripts ["box.js"]

supposedOf :: Box -> Maybe [(Verb, Text)]
supposedOf box = (.supposed) <$> box.play

-- | Returns a link that starts playing the box, or stops playing it when
-- played.
playToggle :: Box -> Html ()
playToggle box = case box.play of
  Nothing ->
    a_
      [class_ "tool", href_ (link (BoxAt box.point box.dead (Just []))), helpOf playHelp]
      "play"
  Just _ -> a_ [class_ "tool", href_ (link (BoxAt box.point box.dead Nothing))] "stop playing"

playHelp :: Text
playHelp = "Suppose accepts and defeats by clicking options, see what each moves, undo. Nothing is recorded until you record."

-- | Returns the suppositions in order, each removable, those past the ones
-- drawn marked, with undo and reset, and each set of rulings reaching no
-- design.
playHtml :: Box -> Play -> Html ()
playHtml box play = div_ [class_ "play"] $ do
  span_ [class_ "lead"] "supposing"
  when (null play.supposed) $ span_ [class_ "muted"] "nothing yet"
  forM_ (zip [0 ..] play.supposed) $ \(i, (v, t)) ->
    a_
      [ class_ ("tool" <> if i < play.drawn then "" else " conflicting")
      , href_ (playLink (deleteAt i play.supposed))
      , helpOf "Remove."
      ]
      (toHtml (verbName v <> " " <> t))
  unless (null play.supposed) $ do
    a_ [class_ "tool", href_ (playLink (init play.supposed))] "undo"
    a_ [class_ "tool", href_ (playLink [])] "reset"
    a_ [class_ "tool", href_ (link (WhatifAt box.point play.supposed))] "whatif"
    when (null play.conflict) $
      a_ [class_ "tool", href_ (link (RecordAt box.point play.supposed)), helpOf recordHelp] "record"
  unless (null play.conflict) $ div_ [class_ "conflict"] $ do
    p_ "these suppositions reach no design. The box shows the ones before the first that conflicts. No design holds:"
    forM_ play.conflict $ \core -> ul_ (forM_ (Set.toList core) (li_ . chunksHtml box.point . Render.rulingChunks box.model box.point))
  p_
    [class_ "muted"]
    "Click an option to accept it, again to defeat it, again to clear it. Outlined: what the last click moved."
  where
    playLink ws = link (BoxAt box.point box.dead (Just ws))

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i + 1) xs

recordHelp :: Text
recordHelp = "Record these as rulings at the point, each with a reason."

-- | Returns the form recording the suppositions as rulings, with a reason
-- for each, the revision it was drawn at and, while the space is locked
-- for an edit, a box overriding the lock.
recordHtml :: Revision -> LockState -> Text -> [(Verb, Text)] -> Html ()
recordHtml rev lock p ws = do
  h1_ (toHtml ("record at " <> p))
  p_
    [class_ "muted"]
    (toHtml ("Each becomes a ruling at " <> p <> ", as accept and defeat record one. A defeat needs a reason."))
  form_ [method_ "post", action_ (path ["p", p, "rule"]), class_ "record"] $ do
    input_ [type_ "hidden", name_ "revision", value_ (revisionText rev)]
    when (Files.isLocked lock) $
      label_ (input_ [type_ "checkbox", name_ "force"] >> " record while the space is locked")
    forM_ ws $ \(v, t) -> label_ $ do
      input_ [type_ "hidden", name_ (verbName v), value_ t]
      span_ (toHtml (verbName v <> " ") >> idLinkFor p t)
      input_ ([type_ "text", name_ "reason", placeholder_ "reason"] ++ [required_ "" | v == DefeatVerb])
    button_ [type_ "submit"] "record"
  p_ (backToPlaying p ws)

-- | Returns the id linked to its page: an option's, or a question's for an
-- id naming one.
idLinkFor :: Text -> Text -> Html ()
idLinkFor p t = a_ [class_ "id", href_ (path ["p", p, "go"] <> query [("id", t)])] (toHtml t)

-- | Returns what recording moved, or why nothing was written.
recordedHtml :: Text -> [(Verb, Text)] -> Either NotRecorded [Step] -> Html ()
recordedHtml p ws (Left why) = do
  h1_ "nothing recorded"
  pre_ (toHtml (notRecordedText p why))
  p_ (backToPlaying p ws)
recordedHtml p _ (Right steps) = do
  h1_ (toHtml ("recorded at " <> p))
  pre_ (toHtml (Text.unlines (concatMap (Render.stepLines p) steps)))
  p_ $ do
    a_ [class_ "tool", href_ (link (BoxAt p HideDead Nothing))] "box"
    " "
    a_ [class_ "tool", href_ (link (BoxAt p HideDead (Just [])))] "play again"

notRecordedText :: Text -> NotRecorded -> Text
notRecordedText _ SpaceChanged = "the space changed since the page was drawn; nothing recorded\n"
notRecordedText _ (UnknownId t) = "unknown id " <> t <> "\n"
notRecordedText _ (NoReason t) = "defeat " <> t <> " needs a reason\n"
notRecordedText _ (EditLocked l) = "the space is " <> Render.locked l <> "; nothing recorded\n"
notRecordedText p (RulingRefused m r) = Render.refusal m p r

backToPlaying :: Text -> [(Verb, Text)] -> Html ()
backToPlaying p ws = a_ [class_ "tool", href_ (link (BoxAt p HideDead (Just ws)))] "back to playing"

-- | What a box page gives each cell: the attributes of its constraints,
-- the link a click follows, and the options supposed.
data CellContext = CellContext
  { point :: Text
  , attrs :: Map OptionId [Attributes]
  , href :: OptionId -> Text
  , supposed :: Set Text
  }

cellContext :: Box -> CellContext
cellContext box =
  CellContext
    { point = box.point
    , attrs = constraintData box.model.space
    , href = maybe (link . OptionAt box.point . optionName) (\play -> playLink play.supposed) box.play
    , supposed = Set.fromList (maybe [] (map snd . (.supposed)) box.play)
    }
  where
    playLink ws o = link (BoxAt box.point box.dead (Just (cycled ws (optionName o))))

-- | Returns the suppositions with the option's next one, last: accepted
-- when it had none, defeated when accepted, none when defeated.
cycled :: [(Verb, Text)] -> Text -> [(Verb, Text)]
cycled ws o = [w | w@(_, t) <- ws, t /= o] ++ next (lookup o [(t, v) | (v, t) <- ws])
  where
    next Nothing = [(AcceptVerb, o)]
    next (Just AcceptVerb) = [(DefeatVerb, o)]
    next (Just _) = []

legend :: [(Text, Html ())]
legend =
  [ ("self", "it")
  , ("needs", "needs")
  , ("needs-one-of", "needs one of")
  , ("needed-by", "needed by")
  , ("excludes", "excludes")
  , ("raised", "raised")
  ]

boxRowHtml :: CellContext -> BoxRow -> Html ()
boxRowHtml context r = div_ [class_ ("box-row" <> movedClass r.was), data_ "key" (Cytoscape.questionKey r.question.id)] $ do
  div_ [class_ "box-question"] $ do
    questionLink context.point r.question.id
    span_ [class_ "title"] (toHtml r.question.title)
    forM_ r.was (\st -> span_ [class_ "badge state"] (toHtml ("was " <> Render.groupTitle (groupKind st))))
  div_ [class_ "cells"] (mapM_ (cellHtml context) r.cells)

cellHtml :: CellContext -> Cell -> Html ()
cellHtml context c =
  a_
    ( [ class_
          ( "cell badge "
              <> markClass (Render.mark c.ruling (Just c.verdict))
              <> movedClass c.was
              <> supposedClass
              <> tintedClass color
          )
      , href_ (context.href c.option)
      , title_
          (optionName c.option <> maybe "" (": " <>) c.title <> maybe "" (\v -> " (was " <> Render.verdictText v <> ")") c.was)
      , data_ "key" (Cytoscape.optionKey c.option)
      ]
        ++ map gradeStyle (maybeToList color)
        ++ Map.findWithDefault [] c.option context.attrs
    )
    (toHtml (Render.key c.option) >> chunksHtml context.point (Render.rankChunks c.rank))
  where
    color = c.grade >>= (.color)
    supposedClass
      | Set.member (optionName c.option) context.supposed = " supposed"
      | otherwise = ""

movedClass :: Maybe a -> Text
movedClass = maybe "" (const " moved")

-- | Returns, for each option, data attributes holding the keys of the
-- options it needs, needs one of and excludes and of the questions it
-- raises.
constraintData :: Space -> Map OptionId [Attributes]
constraintData space = Map.map attributes (Map.fromListWith (Map.unionWith (<>)) (relations ++ raises))
  where
    relations =
      [ (o, keyed (Cytoscape.relationClass e.relation) (Cytoscape.optionKey e.target))
      | (c, _) <- Compile.constraints space
      , (o, e) <- Graph.edgesOf c
      ]
    raises =
      [ (r, keyed "raises" (Cytoscape.questionKey q.id))
      | q <- Map.elems space.questions
      , rs <- q.raisedBy
      , r <- toList rs
      ]
    keyed name k = Map.singleton name (Set.singleton k)
    attributes = map (\(name, ks) -> data_ name (Text.unwords (Set.toList ks))) . Map.toList

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
