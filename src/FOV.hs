module FOV where

import Coord.Types
import Types (Level)
import Level
import qualified Data.List as L
import qualified Data.Set as S

type RaySet = S.Set [Coord]

-- This is a simple raycasting FOV algorithm 
-- a) Precompute a set of rays from (0, 0) for a radius
-- b) when an FOV is needed, follow each ray, marking cells as visible,
--    until a wall is hit

-- Given an angle in radians, a unit vector
unitVector :: Double -> (Double, Double)
unitVector θ = (cos θ, sin θ)

-- Given an angle in radians and a length, the Coord of the cell containing
-- the end point, relative to (0,0)
endPoint :: Double -> Double -> Coord
endPoint θ len = (truncate (x * len), truncate (y * len))
  where (x, y) = unitVector θ

-- Given an angle in radians and a length, the list of Coord for a line
-- of the given length, starting at (0,0)
ray :: Double -> Double -> [Coord]
ray θ len = L.nub $ map (endPoint θ) [0.0,0.125 .. len]

-- Given a radius, compute the set of rays for a field of view
fovRays :: Int -> RaySet
fovRays len = foldl (flip S.insert) S.empty rays
  where
    len' = fromIntegral len - 0.5
    dθ = 2 * pi / 180
    rays = map (flip ray len') [0.0,dθ..2.0*pi]

-- Given a coordinate, a set of rays, and a level,
-- compute the field of view at the coordinate
fov :: Coord -> RaySet -> Level -> [Coord]
fov off rays lvl = foldl maybeAdd [] rays
  where
    maybeAdd result [] = result
    maybeAdd result (x:xs) =
      if isWall coord lvl
        then result'
        else maybeAdd result' xs
      where
        result' = (coord : result)
        coord = x |+| off

