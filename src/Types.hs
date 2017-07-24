module Types where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

data Tile
  = Acid
  | Dr Door
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
  , lMax :: Coord
  , lTiles :: M.Map Coord Tile
  , lChanged :: S.Set Coord
  }

data World = World
  { wHero :: Hero
  , wLevel :: Level
  }

emptyLevel :: Level
emptyLevel =
  Level
  { lDepth = 0
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

unchangedWorld :: World -> World
unchangedWorld world@(World _ level) =
  world { wLevel = level { lChanged = S.empty }}
