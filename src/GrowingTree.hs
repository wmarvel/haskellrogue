module GrowingTree where

import System.Random
import System.Random.Shuffle
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

instance Show Cell where
  show Empty = " "
  show Wall = "#"
  show Floor = "."
  show Connector = "+"

instance Show Grid where
  show g@(Grid (_, y) (_, y') _) = foldl appendRow "" [y .. y']
    where
      appendRow s y'' = s ++ (showGridRow g y'')

showGridRow :: Grid -> Int -> String
showGridRow g@(Grid (x, _) (x', _) _) y =
  foldl cellToString "\n" [(x'', y) | x'' <- [x..x']]
    where cellToString s c = s ++ (show $ lookupCell c g)

emptyGrid :: Grid
emptyGrid = Grid {gMin = (0, 0), gMax = (10, 5), gCells = M.empty}

gridCells :: Grid -> [Coord]
gridCells (Grid (x, y) (x', y') _) =
  [(x'', y'') | x'' <- [x .. x'], y'' <- [y .. y']]

surroundCells :: Coord -> [Coord]
surroundCells (x, y) =
  [ (x', y')
  | x' <- [x - 1 .. x + 1]
  , y' <- [y - 1 .. y + 1]
  , (x, y) /= (x', y')
  ]

adjacentCells :: Coord -> [Coord]
adjacentCells (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

lookupCell :: Coord -> Grid -> Cell
lookupCell x g = M.findWithDefault Empty x $ gCells g

updateCell :: Coord -> Cell -> Grid -> Grid
updateCell x c g = g { gCells = M.insert x c $ gCells g }

cellsOf :: (Cell -> Bool) -> [Coord] -> Grid -> [Coord]
cellsOf f l g = filter predicate l
  where predicate x = isInbounds g x && (f $ lookupCell x g)

surroundsOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
surroundsOf f x g = cellsOf f (surroundCells x) g

adjacentsOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
adjacentsOf f x g = cellsOf f (adjacentCells x) g

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
mazifyGrid g (x:xs) =
  if isCuttable x g
    then do
      exposed <- shuffleM $ exposedCells x g
      mazifyGrid (updateCell x Floor g) (foldl (flip (:)) xs exposed)
    else mazifyGrid (updateCell x Wall g) xs

randomizeGrid :: Grid -> IO Grid
randomizeGrid g = do
  xMaybe <- randomEmptyCell g
  case xMaybe of
    Nothing -> pure g
    Just x -> mazifyGrid g [x]

randomGrid :: IO Grid
randomGrid = randomizeGrid emptyGrid
