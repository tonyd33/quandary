-- | Default colours for grades that declare none.
module Quandary.Core.GradeRamp (rampColor) where

import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showHex)

-- | Returns the colour at the position among the number of grades, from
-- orange at the lowest to blue at the highest.
rampColor :: Int -> Int -> Text
rampColor n i = hex (zipWith (\a b -> a + (b - a) * f) (stops !! k) (stops !! (k + 1)))
  where
    t = if n <= 1 then 1 else fromIntegral i / fromIntegral (n - 1) :: Double
    x = t * fromIntegral (length stops - 1)
    k = min (length stops - 2) (floor x)
    f = x - fromIntegral k

stops :: [[Double]]
stops = [[0xb3, 0x40, 0x1a], [0xc7, 0x7a, 0x12], [0x4f, 0x7b, 0xc4], [0x1d, 0x4e, 0x9e]]

hex :: [Double] -> Text
hex = Text.pack . ('#' :) . concatMap (pad . flip showHex "" . (round :: Double -> Int))
  where
    pad s = replicate (2 - length s) '0' ++ s
