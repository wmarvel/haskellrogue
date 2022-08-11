module Misc where

import Coord.Types
import qualified Data.List as L

-- Should be split out into more appropriate modules later

-- Distance formula
distance :: Coord -> Coord -> Double
distance (x, y) (x', y') = sqrt $ fromIntegral $ (leg * leg) + (leg' * leg')
  where
    leg = x' - x
    leg' = y' - y

-- adapted from https://wiki.haskell.org/Bresenham's_line_drawing_algorithm
line :: Coord -> Coord -> [Coord]
line pa@(xa, ya) pb@(xb, yb) = map maySwitch . L.unfoldr go $ (x1, y1, 0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch =
        if steep
            then (\(x, y) -> (y, x))
            else id
    [(x1, y1), (x2, y2)] = L.sort [maySwitch pa, maySwitch pb]
    dx = x2 - x1
    dy = abs (y2 - y1)
    yInc =
        if y1 < y2
            then 1
            else -1
    go (xTemp, yTemp, e)
        | xTemp > x2 = Nothing
        | otherwise = Just ((xTemp, yTemp), (xTemp + 1, yNew, eNew))
      where
        tempError = e + dy
        (yNew, eNew) =
            if (2 * tempError) >= dx
                then (yTemp + yInc, tempError - dx)
                else (yTemp, tempError)
