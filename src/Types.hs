module Types where

import qualified Data.Map as M

type Coord = (Int, Int)

data Villian = Villian
  { vCurPos :: Coord
  , vOldPos :: Coord
  , vGold :: Int
  , vHP :: Int
  , vItems :: [Item]
  }

data Item
  = Arm Armor
  | Pot Potion
  | Weap Weapon

data Armor = Armor
  { aDefense :: Int
  , aDesc :: String
  }

data Potion = Potion
  { pValue :: String
  , pDesc :: String
  , pEffect :: Effect
  }

data Effect = Harm | Heal

data Weapon = Weapon
  { wDamage :: Int
  , wDesc :: String
  , wToHit :: Int
  }


data Tile
  = Acid
  | Dr Door
  | St Stairs
  | Wall
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
  , hGold :: Int
  , hHP :: Int
  , hItems :: [Item]
  , hWield :: Weapon
  , hWears :: Armor
  }

data Level = Level
  { lDepth :: Int
  , lMax :: Coord
  , lGold :: M.Map Coord Int
  , lItems :: M.Map Coord Item
  , lMapped :: M.Map Coord Bool
  , lTiles :: M.Map Coord Tile
  , lVillians :: M.Map Coord Villian
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
  , lGold = M.empty
  , lItems = M.empty
  , lMapped = M.fromList [((1, 1), True)]
  , lTiles = M.empty
  , lVillians = M.empty
  }

fists :: Weapon
fists = Weapon 0 "Bare fists" 0

rags :: Armor
rags = Armor 0 "Rags"

commoner :: Hero
commoner =
  Hero
  { hCurPos = (1, 1)
  , hOldPos = (1, 1)
  , hGold = 0
  , hHP = 10
  , hItems = []
  , hWield = fists
  , hWears = rags
  }

makeWorld :: World
makeWorld = World {wHero = commoner, wLevel = emptyLevel}
