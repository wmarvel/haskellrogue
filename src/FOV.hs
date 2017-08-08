module FOV where

import Coord.Types
import Types (Level)
import Level

import qualified Data.List as L

-- This is going to be dumb and slow initially.
-- When I can get/write a good trie implementation it will be better
-- but for now, we will generate a list of all rays in a radius starting
-- at 0, 0 and when we need a FOV we will just map the hero's hCurPos
-- over that precomputed list of rays, marking as visible as we go, until
-- we hit a wall on that ray.
zline :: Coord -> [Coord]
zline c = maybeReverse $ line (0, 0) c
  where
    maybeReverse xs@((0, 0):_) = xs
    maybeReverse xs = reverse xs
    

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
      
-- compute a list of rays from (0,0) to points on the radius
fovRays :: Int -> [[Coord]]
fovRays r = map dline $ fovPoints r
  where
    dr = (fromIntegral :: Int -> Double) r
    fint z = (fromIntegral :: Int -> Double) z
    dline coord = filter distance $ zline coord
    distance (x, y) = sqrt (fint (abs x + abs y)) < dr

fovPoints :: Int -> [Coord]
fovPoints r = [(x, y) | x <- [-r..r], y <- [-r..r], abs y == r || abs x == r]

fov :: Coord -> [[Coord]] -> Level -> [Coord]
fov off rays lvl = foldl maybeAdd [] rays
  where
    realCoord p = p |+| off
    maybeAdd result [] = result
    maybeAdd result (x:xs) =
      if isWall coord lvl
        then (coord : result)
        else maybeAdd (coord : result) xs
      where
        coord = realCoord x
