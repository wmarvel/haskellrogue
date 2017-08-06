module Types where

import Coord.Types
import qualified Data.Map as M
import qualified Data.Set as S

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
commoner = Hero {hCurPos = (1, 1), hOldPos = (1, 1)}

makeWorld :: World
makeWorld = World {wHero = commoner, wLevel = emptyLevel}
