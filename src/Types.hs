module Types where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type TileMap = M.Map Coord Tile
type CoordSet = S.Set Coord

data Tile
  = Dr Door
  | St Stairs
  | Wall
  | Floor
  deriving Eq

data Door
  = Closed
  | Opened
  deriving Eq

data Stairs
  = Up
  | Down
  deriving Eq

data Command
  = Move Direction
  | Operate Direction
  | Exit

data Direction
  = North
  | NEast
  | East
  | SEast
  | South
  | SWest
  | West
  | NWest
  | Stand

data Hero = Hero
  { hCurPos :: Coord
  , hOldPos :: Coord
  }

data Level = Level
  { lDepth :: Int
  , lMin :: Coord
  , lMax :: Coord
  , lTiles :: TileMap
  , lChanged :: CoordSet
  }

data Screen = Screen
  { sOffset :: Coord
  , sSize :: (Int, Int)
  }

data World = World
  { wHero :: Hero
  , wLevel :: Level
  }

(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

bestCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
bestCoord f (x, y) (x', y') = (f x x', f y y')

minCoord :: Coord -> Coord -> Coord
minCoord = bestCoord min

maxCoord :: Coord -> Coord -> Coord
maxCoord = bestCoord max

emptyLevel :: Level
emptyLevel =
  Level
  { lDepth = 0
  , lMin = (0, 0)
  , lMax = (1, 1)
  , lTiles = M.empty
  , lChanged = S.empty
  }

commoner :: Hero
commoner =
  Hero
  { hCurPos = (1, 1)
  , hOldPos = (1, 1)
  }

makeWorld :: World
makeWorld = World {wHero = commoner, wLevel = emptyLevel}
