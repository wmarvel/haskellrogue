module Coord.Types where

type Coord = (Int, Int)

(|+|) :: Coord -> Coord -> Coord
(|+|) (x, y) (x', y') = (x + x', y + y')

(|-|) :: Coord -> Coord -> Coord
(|-|) (x, y) (x', y') = (x - x', y - y')

bestCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
bestCoord f (x, y) (x', y') = (f x x', f y y')

minCoord :: Coord -> Coord -> Coord
minCoord = bestCoord min

maxCoord :: Coord -> Coord -> Coord
maxCoord = bestCoord max
