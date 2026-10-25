-- | A space as a directory of Markdown files with YAML frontmatter.
module Quandary.Shell.Storage.Files
  ( FileProblem (..)
  , FileIssue (..)
  , FormatIssue (..)
  , Shape (..)
  , Loaded (..)
  , readSpace
  , directories
  , usableModel
  , problemsOf
  , writePoint
  , removePoint
  , pathOf
  , locate
  )
where

import CMark qualified as M
import Control.Monad (forM, forM_, unless)
import Data.ByteString qualified as ByteString
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List qualified as CL
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Quandary.Core.Source
import Quandary.Core.Types
import Quandary.Core.Validate (Model, Validation (..))
import Quandary.Shell.Storage.Yaml
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.FilePath
  ( dropExtension
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , takeExtension
  , (<.>)
  , (</>)
  )
import Text.Libyaml qualified as Y

data FileProblem = FileProblem
  { file :: FilePath
  , line :: Maybe Int
  , issue :: FileIssue
  }
  deriving (Eq, Show)

data FileIssue
  = Format FormatIssue
  | Semantic Issue
  deriving (Eq, Show)

data FormatIssue
  = NotUtf8
  | NoFrontmatter
  | UnclosedFrontmatter
  | Yaml YamlIssue
  | UnknownKey Text
  | Expected Shape
  deriving (Eq, Show)

data Shape = TextShape | ListShape | MappingShape
  deriving (Eq, Show)

data Loaded = Loaded
  { problems :: [FileProblem]
  , sources :: Sources
  }
  deriving (Eq, Show)

-- | Returns the model, unless some file is unreadable or a problem blocks it.
usableModel :: Loaded -> Validation -> Maybe Model
usableModel loaded v
  | null loaded.problems = v.model
  | otherwise = Nothing

-- | Returns the problems reading the space and validating it, located
-- under the root given.
problemsOf :: FilePath -> Loaded -> Validation -> [FileProblem]
problemsOf root loaded v = loaded.problems ++ map (locate root) v.problems

questionsDir, notesDir, pointsDir :: FilePath
questionsDir = "questions"
notesDir = "notes"
pointsDir = "points"

-- | The directories under the space's root holding its Markdown files.
directories :: [FilePath]
directories = [questionsDir, notesDir, pointsDir]

-- | Read the space's `quandary.yaml` and every Markdown file under its
-- `questions/`, `notes/` and `points/`. A missing file or directory holds
-- nothing.
readSpace :: FilePath -> IO Loaded
readSpace root = do
  (cs, config) <- readConfig root
  qs <- documents root questionsDir QuestionDoc question
  ns <- documents root notesDir NoteDoc note
  ps <- documents root pointsDir PointDoc point
  pure
    Loaded
      { problems = cs ++ concatMap fst qs ++ concatMap fst ns ++ concatMap fst ps
      , sources = Sources {config, questions = map snd qs, notes = map snd ns, points = map snd ps}
      }

readConfig :: FilePath -> IO ([FileProblem], ConfigSource)
readConfig root = do
  exists <- doesFileExist path
  if not exists
    then pure ([], ConfigSource {grades = Nothing})
    else do
      (yamlIssues, yaml) <- parseYaml =<< ByteString.readFile path
      let (issues, c) = config (fromMaybe (Mapping 1 []) yaml)
      pure ([FileProblem (normalise path) (Just l) (Format i) | (l, i) <- [(l, Yaml i) | (l, i) <- yamlIssues] ++ issues], c)
  where
    path = pathOf root ConfigDoc
    config y = do
      m <- fields ["grades"] y
      grades <- textsField ConfigDoc "grades" m
      pure ConfigSource {grades}

documents ::
  FilePath -> FilePath -> (Text -> Document) -> (Document -> Yaml -> Body -> Text -> Decode a) -> IO [([FileProblem], a)]
documents root dir kind decode = do
  paths <- markdownUnder (root </> dir)
  forM paths $ \path -> do
    let doc = kind (Text.intercalate "/" (map Text.pack (splitDirectories (dropExtension (makeRelative (root </> dir) path)))))
    (issues, decoded) <- readDocument doc decode =<< ByteString.readFile path
    pure ([FileProblem (normalise path) (Just l) (Format i) | (l, i) <- issues], decoded)

markdownUnder :: FilePath -> IO [FilePath]
markdownUnder dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- sort <$> listDirectory dir
      fmap concat . forM entries $ \e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        if isDir
          then markdownUnder path
          else pure [path | takeExtension path == ".md"]

-- | Returns the file holding a document.
pathOf :: FilePath -> Document -> FilePath
pathOf root =
  normalise . \case
    QuestionDoc i -> root </> questionsDir </> Text.unpack i <.> "md"
    NoteDoc i -> root </> notesDir </> Text.unpack i <.> "md"
    PointDoc n -> root </> pointsDir </> Text.unpack n <.> "md"
    ConfigDoc -> root </> "quandary.yaml"

locate :: FilePath -> Problem -> FileProblem
locate root p = FileProblem (pathOf root p.at.document) p.at.line (Semantic p.issue)

type Decode = (,) [(Int, FormatIssue)]

complain :: Int -> FormatIssue -> Decode ()
complain l i = ([(l, i)], ())

-- | Decode one file, its frontmatter and body.
readDocument ::
  Document -> (Document -> Yaml -> Body -> Text -> Decode a) -> ByteString.ByteString -> IO ([(Int, FormatIssue)], a)
readDocument doc decode bytes = case decodeUtf8' bytes of
  Left _ -> pure (decode doc (Mapping 1 []) emptyBody "" `with` [(1, NotUtf8)])
  Right content -> case Text.lines content of
    "---" : rest -> case break (== "---") rest of
      (front, _ : after) -> do
        let bodyLine = length front + 3
            body = Text.unlines after
        (yamlIssues, yaml) <- parseYaml (encodeUtf8 (Text.unlines front))
        let shifted = [(l + 1, Yaml i) | (l, i) <- yamlIssues]
            tree = shiftYaml 1 (fromMaybe (Mapping 1 []) yaml)
        pure (decode doc tree (bodyOf doc bodyLine body) body `with` shifted)
      (_, []) -> pure (decode doc (Mapping 1 []) emptyBody "" `with` [(1, UnclosedFrontmatter)])
    _ -> pure (decode doc (Mapping 1 []) (bodyOf doc 1 content) content `with` [(1, NoFrontmatter)])
  where
    (issues, a) `with` more = (more ++ issues, a)
    emptyBody = Body "" []

shiftYaml :: Int -> Yaml -> Yaml
shiftYaml n (Scalar l t) = Scalar (l + n) t
shiftYaml n (Sequence l ys) = Sequence (l + n) (map (shiftYaml n) ys)
shiftYaml n (Mapping l kvs) = Mapping (l + n) [(k + n, key, shiftYaml n v) | (k, key, v) <- kvs]

-- | Split a body at its top-level headings of level one and two.
bodyOf :: Document -> Int -> Text -> Body
bodyOf doc firstLine content =
  Body
    { preamble = slice 1 (fromMaybe (length ls + 1) (listToMaybe starts))
    , sections =
        [ Section
            { heading = Located (Loc doc (Just (start + firstLine - 1))) title
            , level
            , text = slice (end + 1) next
            }
        | ((start, end, level, title), next) <- zip headings (drop 1 starts ++ [length ls + 1])
        ]
    }
  where
    ls = Text.lines content
    M.Node _ _ blocks = M.commonmarkToNode [] content
    headings =
      [ (p.startLine, p.endLine, level, Text.strip (inlineText n))
      | n@(M.Node (Just p) (M.HEADING level) _) <- blocks
      , level <= 2
      ]
    starts = [start | (start, _, _, _) <- headings]
    slice from to = Text.unlines (take (to - from) (drop (from - 1) ls))

inlineText :: M.Node -> Text
inlineText (M.Node _ t children) = case t of
  M.TEXT s -> s
  M.CODE s -> s
  M.SOFTBREAK -> " "
  _ -> Text.concat (map inlineText children)

-- | The keys a mapping may hold, each with its value.
fields :: [Text] -> Yaml -> Decode (Map Text Yaml)
fields allowed (Mapping _ kvs) = do
  forM_ kvs $ \(l, k, _) -> unless (k `elem` allowed) (complain l (UnknownKey k))
  pure (Map.fromList [(k, v) | (_, k, v) <- kvs, k `elem` allowed])
fields _ y = Map.empty <$ complain (lineOf y) (Expected MappingShape)

located :: Document -> Int -> a -> Located a
located doc l = Located (Loc doc (Just l))

textField :: Document -> Text -> Map Text Yaml -> Decode (Maybe (Located Text))
textField doc k m = traverse (text doc) (Map.lookup k m)

text :: Document -> Yaml -> Decode (Located Text)
text doc (Scalar l t) = pure (located doc l t)
text doc y = located doc (lineOf y) "" <$ complain (lineOf y) (Expected TextShape)

textsField :: Document -> Text -> Map Text Yaml -> Decode (Maybe (Located [Located Text]))
textsField doc = listField doc (text doc)

raisersField :: Document -> Map Text Yaml -> Decode (Maybe (Located [Located [Located Text]]))
raisersField doc = listField doc raiser "raised-by"
  where
    raiser (Sequence l ys) = located doc l <$> mapM (text doc) ys
    raiser y = (\t -> located doc (lineOf y) [t]) <$> text doc y

listField :: Document -> (Yaml -> Decode a) -> Text -> Map Text Yaml -> Decode (Maybe (Located [a]))
listField doc item k m = case Map.lookup k m of
  Nothing -> pure Nothing
  Just (Sequence l ys) -> Just . located doc l <$> mapM item ys
  Just y -> Nothing <$ complain (lineOf y) (Expected ListShape)

entriesOf :: Maybe Yaml -> Decode [(Int, Text, Yaml)]
entriesOf Nothing = pure []
entriesOf (Just (Mapping _ kvs)) = pure kvs
entriesOf (Just y) = [] <$ complain (lineOf y) (Expected MappingShape)

listOf :: Maybe Yaml -> Decode [Yaml]
listOf Nothing = pure []
listOf (Just (Sequence _ ys)) = pure ys
listOf (Just y) = [] <$ complain (lineOf y) (Expected ListShape)

question :: Document -> Yaml -> Body -> Text -> Decode QuestionSource
question doc y body _ = do
  m <- fields ["title", "pick", "raised-by", "options", "forbid", "links"] y
  title <- textField doc "title" m
  pick <- textField doc "pick" m
  raisedBy <- raisersField doc m
  options <- entriesOf (Map.lookup "options" m) >>= mapM (\(l, k, v) -> option doc (located doc l k) v)
  forbid <- listOf (Map.lookup "forbid" m) >>= mapM (forbidEntry doc)
  links <- linksOf doc m
  pure
    QuestionSource
      { id = Located (Loc doc Nothing) (questionId (documentId doc))
      , scope = fst (Text.breakOnEnd "/" (documentId doc))
      , title
      , pick
      , raisedBy
      , options
      , forbid
      , links
      , body
      }

questionId :: Text -> Text
questionId path = case reverse (Text.splitOn "/" path) of
  leaf : dir : rest | leaf == dir -> Text.intercalate "/" (reverse (dir : rest))
  _ -> path

option :: Document -> Located Text -> Yaml -> Decode OptionSource
option doc key y = do
  m <- fields ["title", "needs", "needs-one-of", "excludes", "links", "assessment"] y
  title <- textField doc "title" m
  needs <- textsField doc "needs" m
  needsOneOf <- textsField doc "needs-one-of" m
  excludes <- textsField doc "excludes" m
  links <- linksOf doc m
  assessment <- traverse assess (Map.lookup "assessment" m)
  pure OptionSource {key, title, needs, needsOneOf, excludes, links, assessment}
  where
    assess a = do
      m <- fields ["grade", "unverified", "why"] a
      grade <- textField doc "grade" m
      unverified <- textField doc "unverified" m
      why <- textField doc "why" m
      pure AssessmentSource {at = Loc doc (Just (lineOf a)), grade, unverified, why}

forbidEntry :: Document -> Yaml -> Decode ForbidSource
forbidEntry doc y = do
  m <- fields ["all", "why"] y
  literals <- textsField doc "all" m
  why <- textField doc "why" m
  pure ForbidSource {at = Loc doc (Just (lineOf y)), literals, why}

linksOf :: Document -> Map Text Yaml -> Decode [LinkSource]
linksOf doc m = listOf (Map.lookup "links" m) >>= mapM link
  where
    link (Mapping l kvs) = do
      rel <- traverse (text doc) (lookup3 "rel" kvs)
      to <- traverse (text doc) (lookup3 "to" kvs)
      extra <- forM [(k, v) | (_, k, v) <- kvs, k /= "rel", k /= "to"] $ \(k, v) -> do
        t <- text doc v
        pure (k, t.value)
      pure LinkSource {at = Loc doc (Just l), rel, to, extra}
    link y = LinkSource (Loc doc (Just (lineOf y))) Nothing Nothing [] <$ complain (lineOf y) (Expected MappingShape)
    lookup3 k kvs = listToMaybe [v | (_, k', v) <- kvs, k' == k]

note :: Document -> Yaml -> Body -> Text -> Decode NoteSource
note doc y _ raw = do
  m <- fields ["title", "links"] y
  title <- textField doc "title" m
  links <- linksOf doc m
  pure NoteSource {id = Located (Loc doc Nothing) (documentId doc), title, links, body = Body raw []}

point :: Document -> Yaml -> Body -> Text -> Decode PointSource
point doc y _ body = do
  m <- fields ["extends", "proposed", "accept", "defeat", "drop", "defer", "prefer"] y
  extends <- textField doc "extends" m
  proposed <- textField doc "proposed" m
  accept <- entriesOf (Map.lookup "accept" m) >>= mapM ruling
  defeat <- entriesOf (Map.lookup "defeat" m) >>= mapM ruling
  drop' <- textsField doc "drop" m
  defer <- textsField doc "defer" m
  prefer <- entriesOf (Map.lookup "prefer" m) >>= mapM (\(l, k, v) -> (,) (located doc l k) <$> text doc v)
  pure
    PointSource
      { name = Located (Loc doc Nothing) (documentId doc)
      , extends
      , proposed
      , accept
      , defeat
      , drop = drop'
      , defer
      , prefer
      , body
      }
  where
    ruling (l, k, v) = do
      r <- fields ["title", "reason"] v
      title <- textField doc "title" r
      reason <- textField doc "reason" r
      pure RulingSource {option = located doc l k, title, reason}

documentId :: Document -> Text
documentId (QuestionDoc i) = i
documentId (NoteDoc i) = i
documentId (PointDoc n) = n
documentId ConfigDoc = ""

-- | Write a point to its file, replacing what it held.
writePoint :: FilePath -> Point -> IO ()
writePoint root p = do
  yaml <- runConduitRes (CL.sourceList (pointEvents p) .| Y.encodeWith (Y.setWidth Nothing Y.defaultFormatOptions))
  let path = pathOf root (PointDoc p.name)
  createDirectoryIfMissing True (takeDirectory path)
  ByteString.writeFile path ("---\n" <> yaml <> "---\n" <> encodeUtf8 p.body)

removePoint :: FilePath -> Text -> IO ()
removePoint root name = removeFile (pathOf root (PointDoc name))

pointEvents :: Point -> [Y.Event]
pointEvents p =
  [Y.EventStreamStart, Y.EventDocumentStart]
    ++ mapping
      Y.BlockMapping
      ( [("extends", scalar e) | Just e <- [p.extends]]
          ++ [("proposed", scalar "true") | p.proposed]
          ++ [("accept", rulings p.accept) | not (Map.null p.accept)]
          ++ [("defeat", rulings p.defeat) | not (Map.null p.defeat)]
          ++ [("drop", sequence' [scalar (optionName o) | o <- Set.toList p.drop]) | not (Set.null p.drop)]
          ++ [("defer", sequence' [scalar q | Id q <- Set.toList p.defer]) | not (Set.null p.defer)]
          ++ [ ( "prefer"
               , mapping Y.FlowMapping [(optionName o, scalar (Text.pack (show (rankValue n)))) | (o, n) <- Map.toList p.prefer]
               )
             | not (Map.null p.prefer)
             ]
      )
    ++ [Y.EventDocumentEnd, Y.EventStreamEnd]
  where
    rulings rs = mapping Y.BlockMapping [(optionName o, ruling r) | (o, r) <- Map.toList rs]
    ruling r =
      mapping
        Y.FlowMapping
        ([("title", scalar t) | Just t <- [r.title]] ++ [("reason", scalar (reasonText t)) | Just t <- [r.reason]])
    scalar t = [Y.EventScalar (encodeUtf8 t) Y.NoTag Y.Any Nothing]
    sequence' items = [Y.EventSequenceStart Y.NoTag Y.FlowSequence Nothing] ++ concat items ++ [Y.EventSequenceEnd]
    mapping style kvs =
      [Y.EventMappingStart Y.NoTag style Nothing]
        ++ concat [scalar k ++ v | (k, v) <- kvs]
        ++ [Y.EventMappingEnd]
