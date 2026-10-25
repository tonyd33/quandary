-- | Finding questions and options by their ids, titles and text.
module Quandary.Shell.Search
  ( Target (..)
  , Results (..)
  , Hit (..)
  , Place (..)
  , Snippet (..)
  , targets
  , search
  )
where

import Data.Char qualified as Char
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Core.Rulings qualified as Rulings
import Quandary.Core.Types
import Quandary.Core.Validate (Model (..))
import Quandary.Shell.Fuzzy qualified as Fuzzy

-- | What going to an id opens.
data Target = ToQuestion Id | ToOption OptionId
  deriving (Eq, Ord, Show)

-- | What a query finds, each target in the first group it matches in.
data Results = Results
  { ids :: [Target]
  , titles :: [(Text, Target)]
  -- ^ Each title with its target.
  , texts :: [Hit]
  }
  deriving (Eq, Show)

-- | A target whose text holds the query.
data Hit = Hit
  { target :: Target
  , place :: Place
  , snippet :: Snippet
  }
  deriving (Eq, Show)

data Place = Description | Reason
  deriving (Eq, Show)

-- | The line holding the query's first occurrence, split around it.
data Snippet = Snippet
  { before :: Text
  , matched :: Text
  , after :: Text
  }
  deriving (Eq, Show)

-- | Returns every question and option id, as written, with what it opens.
targets :: Space -> [(Text, Target)]
targets space =
  concat
    [ (idText q.id, ToQuestion q.id) : [(optionName o, ToOption o) | o <- choices q]
    | q <- Map.elems space.questions
    ]

-- | Returns the targets whose ids or titles the query matches fuzzily,
-- best first, then those whose descriptions, or rulings' reasons at the
-- point, hold it as written, ignoring case, most occurrences first.
search :: Model -> Text -> Text -> Results
search m p query
  | Text.null (Text.strip query) = Results [] [] []
  | otherwise = Results {ids, titles, texts}
  where
    ids = map snd (Fuzzy.ranked query (targets m.space))
    titles = unseen snd (Set.fromList ids) (Fuzzy.ranked query (titled m.space))
    texts = unseen (.target) (Set.fromList (ids ++ map snd titles)) (textHits query (searchable m p))

-- | Returns the items whose targets are not in the set, each target once.
unseen :: (a -> Target) -> Set.Set Target -> [a] -> [a]
unseen _ _ [] = []
unseen key seen (x : xs)
  | Set.member (key x) seen = unseen key seen xs
  | otherwise = x : unseen key (Set.insert (key x) seen) xs

-- | Returns every question and listed option's title with its target.
titled :: Space -> [(Text, Target)]
titled space =
  concat
    [ (q.title, ToQuestion q.id) : [(o.title, ToOption (Listed o.id)) | o <- q.options]
    | q <- Map.elems space.questions
    ]

-- | Returns every question's description and option's body, and each
-- ruling's reason at the point, with its target.
searchable :: Model -> Text -> [(Target, Place, Text)]
searchable m p =
  concat
    [ (ToQuestion q.id, Description, q.description)
        : [(ToOption (Listed o.id), Description, o.description) | o <- q.options]
    | q <- Map.elems m.space.questions
    ]
    ++ [ (ToOption o, Reason, reasonText r)
       | (o, (_, _, ruling)) <- Map.toList (Rulings.effective m.points p)
       , r <- maybeToList ruling.reason
       ]

-- | Returns the texts holding the query, ignoring case, most occurrences
-- first.
textHits :: Text -> [(Target, Place, Text)] -> [Hit]
textHits query = map snd . sortOn fst . mapMaybe (hitOf (lower query))

-- | Returns the hit with its rank, when a line of the text holds the
-- lowered query.
hitOf :: Text -> (Target, Place, Text) -> Maybe ((Down Int, Target), Hit)
hitOf lowered (target, place, t) = do
  (line, loweredLine) <- find (Text.isInfixOf lowered . snd) (zip (Text.lines t) (Text.lines loweredText))
  pure
    ( (Down (length (Text.breakOnAll lowered loweredText)), target)
    , Hit {target, place, snippet = snippetIn lowered line loweredLine}
    )
  where
    loweredText = lower t

-- | Returns the line split around the first occurrence of the lowered
-- query in the lowered line.
snippetIn :: Text -> Text -> Text -> Snippet
snippetIn lowered line loweredLine = Snippet {before, matched, after}
  where
    (before, rest) = Text.splitAt (Text.length (fst (Text.breakOn lowered loweredLine))) line
    (matched, after) = Text.splitAt (Text.length lowered) rest

-- | Returns the text with each character lowered, its length kept.
lower :: Text -> Text
lower = Text.map Char.toLower
