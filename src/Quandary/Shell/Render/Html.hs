module Quandary.Shell.Render.Html
  ( document
  , link
  , markdown
  )
where

import Control.Monad (forM_, unless, zipWithM_)
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
      h2_ (toHtml (Render.groupTitle g.kind))
      ul_ $ forM_ g.entries $ \e -> li_ $ do
        questionLink p e.question.id
        toHtml (": " <> e.question.title)
        detailHtml m.space p e.detail
  p_ (toHtml (Render.tshow (Set.size ss) <> " stale of " <> Render.tshow (Map.size m.points) <> " points"))

-- | Returns what status shows beside a question, each option linked.
detailHtml :: Space -> Text -> Detail -> Html ()
detailHtml space p = \case
  Choosing cs -> detail $ do
    commaSeparated [keyLink p c.option >> note (Render.choiceNote space c) | c <- cs.possible]
    unless (null cs.possible || null cs.taken) "; "
    unless (null cs.taken) ("taken: " >> commaSeparated (map (keyLink p) cs.taken))
  RaisedIf rs -> detail ("by " >> commaSeparated (map (raiserHtml p) rs))
  Took [] -> pure ()
  Took os -> detail (commaSeparated (map (keyLink p) os))
  NoDetail -> pure ()
  where
    detail = span_ [class_ "detail"]
    note "" = pure ()
    note t = toHtml (" " <> t)

commaSeparated :: [Html ()] -> Html ()
commaSeparated = sequence_ . intersperse ", "

raiserHtml :: Text -> NonEmpty OptionId -> Html ()
raiserHtml p g = sequence_ (intersperse " with " (map (optionLink p) (toList g)))

questionHtml :: Space -> QuestionView -> Html ()
questionHtml space v = do
  h1_ (toHtml (Render.questionTitle v.analysis v.question))
  p_ $ do
    toHtml ("pick: " <> Render.pickText v.question.pick)
    case v.question.raisedBy of
      [] -> ", a root"
      rs -> ", raised by " >> commaSeparated (map (raiserHtml v.point) rs)
  markdown v.question.description
  table_ [class_ "options"] $ forM_ v.rows $ \r -> do
    tr_ [id_ (anchor r.option)] $ do
      td_ (toHtml (Render.rowMark r))
      td_ (optionLink v.point r.option)
      td_ (toHtml (Render.rowDetail space v.point r))
      td_ (whatifButtons v.point r)
    unless (null (Render.rowNotes space r)) $
      tr_ (td_ "" >> td_ [colspan_ "3", class_ "detail"] (notesHtml space v.point r))
    forM_ r.objections $ \ob ->
      tr_ (td_ "" >> td_ [colspan_ "3"] (objectionHtml v.point ob))
  forM_ v.question.options $ \o -> section_ [id_ ("body-" <> idText o.id)] $ do
    h2_ (optionLink v.point (Listed o.id) >> toHtml (": " <> o.title))
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
  p_ $ do
    "in "
    questionLink v.point v.question.id
    toHtml (": " <> v.question.title <> Render.stateSuffix v.analysis v.question)
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
questionLink p q = a_ [href_ (link (QuestionAt p q))] (toHtml (idText q))

optionLink :: Text -> OptionId -> Html ()
optionLink p o = a_ [href_ (link (OptionAt p (optionName o)))] (toHtml (optionName o))

-- | Returns the option's key linked to its page.
keyLink :: Text -> OptionId -> Html ()
keyLink p o = a_ [href_ (link (OptionAt p (optionName o)))] (toHtml (Render.key o))

-- | Returns the raised question folded under its line, its options inside,
-- each followed by the questions it raises in turn.
objectionHtml :: Text -> Objection -> Html ()
objectionHtml p ob = details_ $ do
  summary_ $ do
    "raises "
    questionLink p ob.raising.question.id
    toHtml (": " <> ob.raising.question.title <> " [" <> Render.stateText ob.state <> "]")
    toHtml (" (" <> Render.tshow (length ob.options) <> " options)")
    coRaisersHtml p ob.raising
  ul_ $ forM_ ob.options $ \c -> li_ $ do
    toHtml (Render.verdictText c.verdict <> " ")
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
    [ "body { font-family: system-ui, sans-serif; max-width: 60rem; margin: 1rem auto; padding: 0 1rem; line-height: 1.4; }"
    , "nav { display: flex; gap: 1rem; margin-bottom: 1rem; }"
    , "pre, code { font-family: ui-monospace, monospace; }"
    , "pre { background: #f4f4f4; padding: 0.5rem; overflow-x: auto; }"
    , "table.options td { padding: 0.2rem 0.5rem; vertical-align: top; }"
    , ".detail { color: #555; margin-left: 0.5rem; }"
    , "form { display: inline; }"
    , "section { border-top: 1px solid #ddd; margin-top: 1rem; }"
    , "details ul { margin: 0.2rem 0; }"
    , "summary { cursor: pointer; }"
    , ".body table { border-collapse: collapse; }"
    , ".body td, .body th { border: 1px solid #ddd; padding: 0.2rem 0.5rem; }"
    ]
