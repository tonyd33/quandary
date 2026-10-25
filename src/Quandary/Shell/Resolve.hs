module Quandary.Shell.Resolve
  ( withOption
  , withQuestion
  , withQuestionOrOption
  , optionWithQuestion
  , reopening
  , Verb (..)
  , verbName
  , verbNamed
  , supposing
  , written
  , optionsThenReason
  , questionsThenReason
  , withReason
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Analyse (Supposition (..))
import Quandary.Core.Rulings
import Quandary.Core.Types
import Quandary.Shell.Exit
import System.Exit (ExitCode)

withOption :: Space -> Text -> (OptionId -> IO ExitCode) -> IO ExitCode
withOption space t k = maybe (failWith (notAnOption space t)) k (resolveOption space t)

withQuestion :: Space -> Text -> (Question -> IO ExitCode) -> IO ExitCode
withQuestion space q k = maybe (failWith (notAQuestion space q)) k (Map.lookup (Id q) space.questions)

-- | Run the continuation on the question with the id, else on the option
-- with its question.
withQuestionOrOption :: Space -> Text -> (Either Question (Question, OptionId) -> IO ExitCode) -> IO ExitCode
withQuestionOrOption space t k = case (Map.lookup (Id t) space.questions, optionWithQuestion space t) of
  (Just q, _) -> k (Left q)
  (Nothing, Just qo) -> k (Right qo)
  (Nothing, Nothing) -> failWith ("unknown question " <> t)

-- | Returns the option with the id and its question.
optionWithQuestion :: Space -> Text -> Maybe (Question, OptionId)
optionWithQuestion space t = do
  o <- resolveOption space t
  q <- questionContaining space o
  pure (q, o)

reopening :: Space -> Text -> Maybe PointEdit
reopening space t = Reopen <$> resolveOption space t <|> Undefer . (.id) <$> Map.lookup (Id t) space.questions

-- | How `whatif` supposes of an id.
data Verb = AcceptVerb | DefeatVerb | ReopenVerb
  deriving (Eq, Ord, Show, Enum, Bounded)

verbName :: Verb -> Text
verbName AcceptVerb = "accept"
verbName DefeatVerb = "defeat"
verbName ReopenVerb = "reopen"

verbNamed :: Text -> Maybe Verb
verbNamed t = find ((== t) . verbName) [minBound ..]

supposing :: Space -> Verb -> Text -> Maybe Supposition
supposing space AcceptVerb t = Suppose . Take <$> resolveOption space t
supposing space DefeatVerb t = Suppose . Refuse <$> resolveOption space t
supposing space ReopenVerb t = Apply <$> reopening space t

-- | Returns the verb and id that suppose it.
written :: Supposition -> Maybe (Verb, Text)
written (Suppose (Take o)) = Just (AcceptVerb, optionName o)
written (Suppose (Refuse o)) = Just (DefeatVerb, optionName o)
written (Apply (Reopen o)) = Just (ReopenVerb, optionName o)
written (Apply (Undefer q)) = Just (ReopenVerb, idText q)
written (Apply _) = Nothing

optionsThenReason :: Space -> [Text] -> Either Text ([OptionId], Maybe Reason)
optionsThenReason space = idsThenReason "an option" (resolveOption space) (notAnOption space)

questionsThenReason :: Space -> [Text] -> Either Text ([Question], Maybe Reason)
questionsThenReason space = idsThenReason "a question" (\t -> Map.lookup (Id t) space.questions) (notAQuestion space)

withReason :: Text -> Either Text ([a], Maybe Reason) -> ([a] -> Reason -> IO ExitCode) -> IO ExitCode
withReason verb parsed k = case parsed of
  Left err -> failWith err
  Right (_, Nothing) -> failWith (verb <> ": the reason is empty")
  Right (xs, Just reason) -> k xs reason

idsThenReason :: Text -> (Text -> Maybe a) -> (Text -> Text) -> [Text] -> Either Text ([a], Maybe Reason)
idsThenReason what resolve unknown args = case resolvedPrefix resolve args of
  ([], t : _) -> Left (unknown t)
  ([], []) -> Left ("expected " <> what)
  (_, t : _) | idLike t -> Left (unknown t)
  (ids, rest) -> Right (ids, reasonOf rest)

resolvedPrefix :: (Text -> Maybe a) -> [Text] -> ([a], [Text])
resolvedPrefix resolve (t : ts) | Just x <- resolve t = first (x :) (resolvedPrefix resolve ts)
resolvedPrefix _ ts = ([], ts)

idLike :: Text -> Bool
idLike t = Text.any (== '/') t && isJust (writtenOption t)

reasonOf :: [Text] -> Maybe Reason
reasonOf ws = parseReason (Text.strip (Text.unwords ws))

notAnOption :: Space -> Text -> Text
notAnOption space t
  | Map.member (Id t) space.questions = t <> " is a question, not an option"
  | otherwise = "unknown option " <> t

notAQuestion :: Space -> Text -> Text
notAQuestion space t
  | isJust (resolveOption space t) = t <> " is an option, not a question"
  | otherwise = "unknown question " <> t
