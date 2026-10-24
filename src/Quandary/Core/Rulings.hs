module Quandary.Core.Rulings
  ( Edit (..)
  , PointEdit (..)
  , EditError (..)
  , PointError (..)
  , current
  , emptyPoint
  , pointNamed
  , newPoint
  , withoutPoint
  , ruledLiteral
  , lineage
  , descendants
  , effective
  , rulingsOf
  , deferredAt
  , preferenceAt
  , byPreference
  , rulingFrom
  , inheritedAt
  , applyEdit
  , withRuling
  , resolveOption
  , writtenOption
  , titleOf
  )
where

import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Types

data Edit
  = Rule PointEdit
  | -- | Defeat every option of the question still possible.
    Settle Question Reason
  deriving (Eq, Show)

data PointEdit
  = Accept OptionId (Maybe Reason)
  | Defeat OptionId Reason
  | Reopen OptionId
  | Undefer Id
  | Defer Id
  | Prefer OptionId (Maybe Rank)
  deriving (Eq, Show)

data EditError
  = NotRuled OptionId
  | NotDeferred Id
  | -- | A settle at a stale point.
    SettlesStale Id
  deriving (Eq, Show)

data PointError
  = PointExists Text
  | NoSuchPoint Text
  | RemovesCurrent
  | -- | The first point extends the second.
    ExtendedBy Text Text
  deriving (Eq, Show)

-- | The point every space has, whether or not its file exists.
current :: Text
current = "current"

-- | Returns the named point, empty when there is none.
emptyPoint :: Text -> Point
emptyPoint name =
  Point
    { name
    , extends = Nothing
    , proposed = False
    , accept = Map.empty
    , defeat = Map.empty
    , drop = Set.empty
    , defer = Set.empty
    , prefer = Map.empty
    , body = ""
    }

pointNamed :: Map Text Point -> Text -> Point
pointNamed points name = Map.findWithDefault (emptyPoint name) name points

-- | Returns an empty point to add to the points, or why it cannot be added.
newPoint :: Map Text Point -> Text -> Maybe Text -> Bool -> Either PointError Point
newPoint points name extends proposed
  | Map.member name points = Left (PointExists name)
  | Just parent <- extends, Map.notMember parent points = Left (NoSuchPoint parent)
  | otherwise = Right (emptyPoint name) {extends, proposed}

-- | Returns the points without the named one, or why it cannot be removed.
withoutPoint :: Map Text Point -> Text -> Either PointError (Map Text Point)
withoutPoint points name
  | name == current = Left RemovesCurrent
  | Map.notMember name points = Left (NoSuchPoint name)
  | Just child <- find (extendsPoint name) (Map.elems points) = Left (ExtendedBy child.name name)
  | otherwise = Right (Map.delete name points)

extendsPoint :: Text -> Point -> Bool
extendsPoint name p = p.extends == Just name

ruledLiteral :: PointEdit -> Maybe Literal
ruledLiteral (Accept o _) = Just (Take o)
ruledLiteral (Defeat o _) = Just (Refuse o)
ruledLiteral _ = Nothing

-- | Returns the named point and the points it extends, nearest first.
--
-- Preconditions: no cycle of `extends`.
lineage :: Map Text Point -> Text -> [Point]
lineage points name = p : maybe [] (lineage points) p.extends
  where
    p = pointNamed points name

-- | Returns the other points whose lineage holds the named one.
--
-- Preconditions: no cycle of `extends`.
descendants :: Map Text Point -> Text -> [Text]
descendants points name = [n | n <- Map.keys points, n /= name, name `elem` map (.name) (lineage points n)]

-- | Returns each ruled option's ruling at the named point and the point
-- it comes from. A point's own rulings replace those of the points it
-- extends, and its drops remove them.
--
-- Preconditions: no cycle of `extends`.
effective :: Map Text Point -> Text -> Map OptionId (Literal, Text, Ruling)
effective points name = foldr layer Map.empty (lineage points name)
  where
    layer p inherited =
      Map.unions
        [ Map.mapWithKey (\o r -> (Take o, p.name, r)) p.accept
        , Map.mapWithKey (\o r -> (Refuse o, p.name, r)) p.defeat
        , Map.withoutKeys inherited p.drop
        ]

rulingsOf :: Map Text Point -> Text -> Set Literal
rulingsOf points name = Set.fromList [l | (l, _, _) <- Map.elems (effective points name)]

deferredAt :: Map Text Point -> Text -> Set Id
deferredAt points name = Set.unions [p.defer | p <- lineage points name]

-- | Returns each ranked option's rank, the nearest point's winning.
preferenceAt :: Map Text Point -> Text -> Map OptionId Rank
preferenceAt points name = Map.unions [p.prefer | p <- lineage points name]

-- | Returns the question's options by preference given the ranks: ranked
-- first, highest rank first, then as listed, the implicit other last.
byPreference :: Map OptionId Rank -> Question -> [OptionId]
byPreference ranks q = sortOn (\o -> Down (Map.lookup o ranks)) (listedThenOther q)

-- | Returns the point the literal's ruling comes from, and the ruling.
rulingFrom :: Map Text Point -> Text -> Literal -> Maybe (Text, Ruling)
rulingFrom points name l = case Map.lookup (optionOf l) (effective points name) of
  Just (l', p, r) | l' == l -> Just (p, r)
  _ -> Nothing

-- | Holds when the literal's ruling at the named point comes from a point
-- it extends.
inheritedAt :: Map Text Point -> Text -> Literal -> Bool
inheritedAt points name l = maybe False ((/= name) . fst) (rulingFrom points name l)

-- | Apply an edit to the named point itself.
applyEdit :: Space -> Map Text Point -> Text -> PointEdit -> Either EditError Point
applyEdit space points name = editedPoint space (effective points name) (pointNamed points name)

editedPoint :: Space -> Map OptionId (Literal, Text, Ruling) -> Point -> PointEdit -> Either EditError Point
editedPoint space _ p (Accept o reason) = pure (withRuling space (Take o) reason p)
editedPoint space _ p (Defeat o reason) = pure (withRuling space (Refuse o) (Just reason) p)
editedPoint _ inherited p (Reopen o)
  | Map.member o p.accept || Map.member o p.defeat =
      pure p {accept = Map.delete o p.accept, defeat = Map.delete o p.defeat}
  | Map.member o inherited = pure p {drop = Set.insert o p.drop}
  | otherwise = Left (NotRuled o)
editedPoint _ _ p (Undefer q)
  | Set.member q p.defer = pure p {defer = Set.delete q p.defer}
  | otherwise = Left (NotDeferred q)
editedPoint _ _ p (Defer q) = pure p {defer = Set.insert q p.defer}
editedPoint _ _ p (Prefer o r) = pure p {prefer = Map.alter (const r) o p.prefer}

withRuling :: Space -> Literal -> Maybe Reason -> Point -> Point
withRuling space (Take o) reason p =
  p {accept = Map.insert o (ruling space o reason) p.accept, defeat = Map.delete o p.defeat, drop = Set.delete o p.drop}
withRuling space (Refuse o) reason p =
  p {defeat = Map.insert o (ruling space o reason) p.defeat, accept = Map.delete o p.accept, drop = Set.delete o p.drop}

ruling :: Space -> OptionId -> Maybe Reason -> Ruling
ruling space o reason = Ruling {title = titleOf space o, reason}

-- | Returns the option a written id names: a listed option, or
-- `<question>.other`.
resolveOption :: Space -> Text -> Maybe OptionId
resolveOption space = \t -> case writtenOption t of
  Just (q, Just "other") | Map.member q space.questions -> Just (Other q)
  Just (i, Nothing) -> (\(_, o) -> Listed o.id) <$> at i
  _ -> Nothing
  where
    at = optionAt space

-- | Returns the id before the first `.` and the text after it; Nothing for an invalid id.
writtenOption :: Text -> Maybe (Id, Maybe Text)
writtenOption t
  | validId i = Just (Id i, Text.stripPrefix "." rest)
  | otherwise = Nothing
  where
    (i, rest) = Text.breakOn "." t

-- | Returns a listed option's title; Nothing for an implicit other.
titleOf :: Space -> OptionId -> Maybe Text
titleOf _ (Other _) = Nothing
titleOf space (Listed i) = (\(_, o) -> o.title) <$> at i
  where
    at = optionAt space
