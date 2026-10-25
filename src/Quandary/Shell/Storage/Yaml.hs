-- | YAML as a tree of text, every scalar kept as spelled.
module Quandary.Shell.Storage.Yaml
  ( Yaml (..)
  , Style (..)
  , YamlIssue (..)
  , parseYaml
  , lineOf
  )
where

import Control.Exception (try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List qualified as CL
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Text.Libyaml qualified as Y

-- | Each node carries its line, counted from 1 in the YAML text.
data Yaml
  = Scalar Int Text
  | Sequence Int [Yaml]
  | Mapping Int Style [(Int, Text, Yaml)]
  deriving (Eq, Show)

-- | Whether a mapping is written in braces or as indented lines.
data Style = Block | Flow
  deriving (Eq, Show)

data YamlIssue
  = Syntax Text
  | Alias
  | Anchor
  | Tag
  | DuplicateKey Text
  | ComplexKey
  deriving (Eq, Show)

lineOf :: Yaml -> Int
lineOf (Scalar l _) = l
lineOf (Sequence l _) = l
lineOf (Mapping l _ _) = l

-- | Returns the document's tree, Nothing for an empty document, and the
-- issues found with their lines.
parseYaml :: ByteString -> IO ([(Int, YamlIssue)], Maybe Yaml)
parseYaml bytes = do
  events <- try (runConduitRes (Y.decodeMarked bytes .| CL.consume))
  pure $ case events of
    Left (Y.YamlParseException problem context mark) ->
      ([(mark.yamlLine + 1, Syntax (Text.pack (unwords (filter (not . null) [problem, context]))))], Nothing)
    Left (Y.YamlException message) -> ([(1, Syntax (Text.pack message))], Nothing)
    Right es -> document es

type Build = (,) [(Int, YamlIssue)]

issue :: Int -> YamlIssue -> Build ()
issue l i = ([(l, i)], ())

document :: [Y.MarkedEvent] -> Build (Maybe Yaml)
document es = case dropWhile framing es of
  [] -> pure Nothing
  rest -> do
    (y, _) <- node rest
    pure (Just y)
  where
    framing e = case e.yamlEvent of
      Y.EventStreamStart -> True
      Y.EventDocumentStart -> True
      Y.EventStreamEnd -> True
      _ -> False

-- | Build the node the events open with, returning the events after it.
--
-- Preconditions: the events are a well-formed stream from the parser.
node :: [Y.MarkedEvent] -> Build (Yaml, [Y.MarkedEvent])
node [] = pure (Scalar 1 "", [])
node (e : rest) = case e.yamlEvent of
  Y.EventScalar bytes tag _ anchor -> do
    properties tag anchor
    pure (Scalar l (decodeUtf8Lenient bytes), rest)
  Y.EventAlias _ -> do
    issue l Alias
    pure (Scalar l "", rest)
  Y.EventSequenceStart tag _ anchor -> do
    properties tag anchor
    (ys, rest') <- items rest
    pure (Sequence l ys, rest')
  Y.EventMappingStart tag style anchor -> do
    properties tag anchor
    (kvs, rest') <- entries Set.empty rest
    pure (Mapping l (styleOf style) kvs, rest')
  _ -> node rest
  where
    l = e.yamlStartMark.yamlLine + 1
    properties tag anchor = do
      when (tag /= Y.NoTag) (issue l Tag)
      forM_ anchor (const (issue l Anchor))

styleOf :: Y.MappingStyle -> Style
styleOf Y.FlowMapping = Flow
styleOf _ = Block

items :: [Y.MarkedEvent] -> Build ([Yaml], [Y.MarkedEvent])
items (e : rest) | Y.EventSequenceEnd <- e.yamlEvent = pure ([], rest)
items es = do
  (y, rest) <- node es
  (ys, rest') <- items rest
  pure (y : ys, rest')

entries :: Set.Set Text -> [Y.MarkedEvent] -> Build ([(Int, Text, Yaml)], [Y.MarkedEvent])
entries _ (e : rest) | Y.EventMappingEnd <- e.yamlEvent = pure ([], rest)
entries seen es = do
  (k, rest) <- node es
  (v, rest') <- node rest
  case k of
    Scalar l key
      | Set.member key seen -> do
          issue l (DuplicateKey key)
          entries seen rest'
      | otherwise -> do
          (kvs, rest'') <- entries (Set.insert key seen) rest'
          pure ((l, key, v) : kvs, rest'')
    _ -> do
      issue (lineOf k) ComplexKey
      entries seen rest'
