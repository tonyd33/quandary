module Quandary.Shell.Fuzzy
  ( score
  , ranked
  )
where

import Data.List (sortOn, zipWith4)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- | Returns how well the query matches the id, Nothing unless its
-- characters appear in the id in order, ignoring case. A matched character
-- scores 1, 8 more at the start of the id or right after a `/`, 6 more right
-- after a `-`, `_` or `.`, and 4 more right after the character matched
-- before it; each character skipped between two matched ones costs 1.
score :: Text -> Text -> Maybe Int
score query t = case Text.unpack (Text.toLower query) of
  [] -> Just 0
  q : qs -> maximum (Nothing : foldl nextRow (row q (repeat (Just 0)) (repeat Nothing)) qs)
  where
    cs = Text.unpack (Text.toLower t)
    bases = 9 : map bonusAfter cs
    nextRow prev c = row c (bestBefore prev) (Nothing : prev)
    row c before adjacent = zipWith4 (matchCell c) cs bases before adjacent

-- | Returns what matching the character right after this one scores.
bonusAfter :: Char -> Int
bonusAfter c
  | c == '/' = 9
  | c `elem` ("-_." :: String) = 7
  | otherwise = 1

-- | Returns, at each position, the best score of a match before it, less
-- one for each character skipped since.
bestBefore :: [Maybe Int] -> [Maybe Int]
bestBefore prev = [subtract (j - 1) <$> b | (j, b) <- zip [0 ..] (scanl max Nothing [(+ k) <$> p | (k, p) <- zip [0 ..] prev])]

-- | Returns the score of matching the query character at the id
-- character, given its base score, the best match before it and the match
-- right before it.
matchCell :: Char -> Char -> Int -> Maybe Int -> Maybe Int -> Maybe Int
matchCell c x b before adjacent
  | x /= c = Nothing
  | otherwise = max ((+ b) <$> before) ((+ (b + 4)) <$> adjacent)

-- | Returns the items whose ids the query matches, best first, then
-- shorter ids, then in order of id.
ranked :: Text -> [(Text, a)] -> [(Text, a)]
ranked query xs = map snd (sortOn fst (mapMaybe scored xs))
  where
    scored x@(t, _) = (\s -> ((Down s, Text.length t, t), x)) <$> score query t
