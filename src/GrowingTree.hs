module GrowingTree where

import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type CellMap = M.Map Coord Cell
type CoordSet = S.Set Coord

data Cell
  = Empty
  | Wall
  | Floor
  | Connector
  deriving Eq

data Grid = Grid
  { gMin :: Coord
  , gMax :: Coord
  , gCells :: CellMap
  }

emptyGrid :: Grid
emptyGrid = Grid {gMin = (0, 0), gMax = (79, 24), gCells = M.empty}

gridCells :: Grid -> [Coord]
gridCells (Grid (x, y) (x', y') _) =
  [(x'', y'') | x'' <- [x .. x'], y'' <- [y .. y']]

adjacentCells :: Coord -> [Coord]
adjacentCells (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

lookupCell :: Coord -> Grid -> Cell
lookupCell x g = M.findWithDefault Empty x $ gCells g

updateCell :: Coord -> Cell -> Grid -> Grid
updateCell x c g = g { gCells = M.insert x c $ gCells g }

adjacentsOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
adjacentsOf f x g = filter predicate (adjacentCells x)
  where predicate x' = isInbounds g x' && (f $ lookupCell x' g)

isInbounds :: Grid -> Coord -> Bool
isInbounds (Grid (xMin, yMin) (xMax, yMax) _) (x, y) =
  xMin <= x && yMin <= y && x <= xMax && y <= yMax

exposedCells :: Coord -> Grid -> [Coord]
exposedCells x g = adjacentsOf (==Empty) x g

isCuttable :: Coord -> Grid -> Bool
isCuttable x g = length (adjacentsOf (==Floor) x g) <= 1

randomElt :: [a] -> IO (Maybe a)
randomElt [] = pure Nothing
randomElt xs = do
  i <- getStdRandom $ randomR (0, length xs - 1)
  pure $ Just $ xs !! i

randomEmptyCell :: Grid -> IO (Maybe Coord)
randomEmptyCell g = randomElt $ filter predicate $ gridCells g
  where predicate x = (lookupCell x g) == Empty

randomExposedCell :: Coord -> Grid -> IO (Maybe Coord)
randomExposedCell x g = randomElt $ exposedCells x g

mazifyGrid :: Grid -> [Coord] -> IO Grid
mazifyGrid g [] = pure g
mazifyGrid g (x:xs) = do
  xMaybe <- randomExposedCell x g
  case xMaybe of
    Just x' ->
      if isCuttable x' g
        then mazifyGrid (updateCell x Floor g) (x' : xs)
        else mazifyGrid (updateCell x Wall g) xs
    Nothing -> mazifyGrid g xs

randomizeGrid :: Grid -> IO Grid
randomizeGrid g = do
  xMaybe <- randomEmptyCell g
  case xMaybe of
    Nothing -> pure g
    Just x -> mazifyGrid(updateCell x Floor g) [x]

randomGrid :: IO Grid
randomGrid = randomizeGrid emptyGrid
