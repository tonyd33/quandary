module Quandary.Core.Types
  ( Id (..)
  , OptionId (..)
  , Lit (..)
  , Literal
  , Pick (..)
  , Space (..)
  , Question (..)
  , Option (..)
  , Assessment (..)
  , Forbid (..)
  , Link (..)
  , Note (..)
  , Point (..)
  , Ruling (..)
  , Reason
  , Rank
  , rank
  , parseRank
  , rankValue
  , maxRank
  , parseReason
  , reasonText
  , Formula (..)
  , Verdict (..)
  , Raising (..)
  , choices
  , optionOf
  , opposite
  , assertedVerdict
  , listedThenOther
  , questionContaining
  , optionName
  , idText
  , validId
  , validSegment
  , optionAt
  , questionOf
  , raisedQuestions
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

newtype Id = Id Text
  deriving (Eq, Ord, Show)

data OptionId
  = Listed Id
  | -- | The implicit option of a question: something not listed.
    Other Id
  deriving (Eq, Ord, Show)

data Lit v
  = Take v
  | Refuse v
  deriving (Eq, Ord, Show, Functor)

type Literal = Lit OptionId

-- | How many of its options a live question takes: exactly one, one or
-- more, or any number.
data Pick = One | Some | Any
  deriving (Eq, Show)

data Space = Space
  { grades :: [Text]
  -- ^ The grades an option may be assessed at, lowest first.
  , questions :: Map Id Question
  , notes :: Map Id Note
  }
  deriving (Eq, Show)

data Question = Question
  { id :: Id
  , title :: Text
  , description :: Text
  , pick :: Pick
  , raisedBy :: [NonEmpty OptionId]
  -- ^ Empty for a root. Each raiser is the options it takes together.
  , options :: [Option]
  , forbid :: [Forbid]
  , links :: [Link]
  }
  deriving (Eq, Show)

data Option = Option
  { id :: Id
  , title :: Text
  , description :: Text
  , needs :: [OptionId]
  , needsOneOf :: Maybe (NonEmpty OptionId)
  , excludes :: [OptionId]
  , links :: [Link]
  , assessment :: Maybe Assessment
  }
  deriving (Eq, Show)

-- | An agent's grade of an option, not a ruling.
data Assessment = Assessment
  { grade :: Text
  , unverified :: Bool
  , why :: Maybe Text
  }
  deriving (Eq, Show)

data Forbid = Forbid
  { literals :: Set Literal
  , why :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Link = Link
  { rel :: Text
  , to :: Id
  , extra :: Map Text Text
  }
  deriving (Eq, Show)

data Note = Note
  { id :: Id
  , title :: Text
  , description :: Text
  , links :: [Link]
  }
  deriving (Eq, Show)

-- | A named set of rulings.
data Point = Point
  { name :: Text
  , extends :: Maybe Text
  , proposed :: Bool
  -- ^ Holds an agent's proposed rulings.
  , accept :: Map OptionId Ruling
  , defeat :: Map OptionId Ruling
  , drop :: Set OptionId
  -- ^ Inherited rulings this point does without.
  , defer :: Set Id
  , prefer :: Map OptionId Rank
  , body :: Text
  }
  deriving (Eq, Show)

data Ruling = Ruling
  { title :: Maybe Text
  -- ^ The option's title when ruled on; Nothing for an implicit other.
  , reason :: Maybe Reason
  }
  deriving (Eq, Show)

-- | Text holding something besides whitespace.
newtype Reason = Reason Text
  deriving (Eq, Show)

parseReason :: Text -> Maybe Reason
parseReason t
  | Text.null (Text.strip t) = Nothing
  | otherwise = Just (Reason t)

reasonText :: Reason -> Text
reasonText (Reason t) = t

-- | A whole number from 0 to `maxRank`.
newtype Rank = Rank Int
  deriving (Eq, Ord, Show)

maxRank :: Int
maxRank = 10

rank :: Int -> Maybe Rank
rank r
  | r >= 0 && r <= maxRank = Just (Rank r)
  | otherwise = Nothing

parseRank :: Text -> Maybe Rank
parseRank t = case reads (Text.unpack t) of
  [(r, "")] | Text.all isDigit t -> rank r
  _ -> Nothing

rankValue :: Rank -> Int
rankValue (Rank r) = r

-- | A design decides every variable and holds no forbidden set. Every
-- forbidden set uses only the variables.
data Formula v = Formula
  { variables :: Set v
  , forbidden :: Set (Set (Lit v))
  }
  deriving (Eq, Show)

-- | An option's standing over the valid designs reaching a point.
data Verdict
  = -- | Every design takes it.
    Forced
  | -- | Some design takes it and some refuses it.
    Possible
  | -- | No design takes it.
    Impossible
  deriving (Eq, Show)

-- | A question some options raise.
data Raising = Raising
  { question :: Question
  , with :: [NonEmpty OptionId]
  -- ^ Each set of further options that raises it alongside them; empty
  -- when they raise it alone.
  }
  deriving (Eq, Show)

-- | Returns the questions some of the options raise.
raisedQuestions :: Space -> Set OptionId -> [Raising]
raisedQuestions space os = mapMaybe (raising os) (Map.elems space.questions)

raising :: Set OptionId -> Question -> Maybe Raising
raising os q
  | null others = Nothing
  | any null others = Just (Raising q [])
  | otherwise = Just (Raising q (mapMaybe nonEmpty (nubOrd others)))
  where
    others = [filter (`Set.notMember` os) (toList g) | g <- q.raisedBy, any (`Set.member` os) g]

-- | Returns the options a design decides for the question, its implicit
-- other first.
choices :: Question -> [OptionId]
choices q = Other q.id : map (\o -> Listed o.id) q.options

optionOf :: Lit v -> v
optionOf (Take o) = o
optionOf (Refuse o) = o

opposite :: Lit v -> Lit v
opposite (Take v) = Refuse v
opposite (Refuse v) = Take v

assertedVerdict :: Lit v -> Verdict
assertedVerdict (Take _) = Forced
assertedVerdict (Refuse _) = Impossible

listedThenOther :: Question -> [OptionId]
listedThenOther q = map (Listed . (.id)) q.options ++ [Other q.id]

-- | Returns the option's id as written: `keys/local`, `keys/where.other`.
optionName :: OptionId -> Text
optionName (Listed (Id t)) = t
optionName (Other (Id q)) = q <> ".other"

idText :: Id -> Text
idText (Id t) = t

-- | Holds for a path of segments of letters, digits, `-` and `_`.
validId :: Text -> Bool
validId = all validSegment . Text.splitOn "/"

validSegment :: Text -> Bool
validSegment t = not (Text.null t) && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '-' || c == '_') t

-- | Returns the listed option with the id, and its question.
optionAt :: Space -> Id -> Maybe (Question, Option)
optionAt space = (`Map.lookup` index)
  where
    index = Map.fromList [(o.id, (q, o)) | q <- Map.elems space.questions, o <- q.options]

-- | Returns the question an option answers.
questionContaining :: Space -> OptionId -> Maybe Question
questionContaining space o = questionOf space o >>= (`Map.lookup` space.questions)

-- | Returns the question an option answers. Bind `questionOf space` once
-- outside a loop; each application to a space indexes every option.
questionOf :: Space -> OptionId -> Maybe Id
questionOf space = owner
  where
    at = optionAt space
    owner (Other q) = Just q
    owner (Listed i) = (\(q, _) -> q.id) <$> at i
