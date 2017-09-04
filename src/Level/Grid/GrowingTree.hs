module Level.Grid.GrowingTree where

import Coord.Types
import Level.Grid.Types
import System.Random
import System.Random.PCG

-- Using FRecBT as the frontier results in a recursive backtracker
newtype FRecBT = FRecBT [Coord]

-- Using FPrim as the frontier results in Prim's algorithm
newtype FPrim = FPrim [Coord]

data FOldest = FOldest [Coord] [Coord]

-- Type for supported algorithms
data MazeAlgo = MazeRecBT | MazePrim | MazeOldest

class Frontier a where
  fadd :: Coord -> a -> a
  fremove :: Coord -> a -> a
  fselect :: a -> IO (Maybe Coord)

instance Frontier FRecBT where
  fadd c (FRecBT cs) = FRecBT $ c : cs
  fremove c (FRecBT cs) = FRecBT $ filter (/=c) cs
  fselect (FRecBT []) = pure Nothing
  fselect (FRecBT cs) = pure $ Just $ head cs

instance Frontier FPrim where
  fadd c (FPrim cs) = FPrim $ c : cs
  fremove c (FPrim cs) = FPrim $ filter (/=c) cs
  fselect (FPrim []) = pure Nothing
  fselect (FPrim cs) = do
    mc <- randomElt cs
    pure mc

instance Frontier FOldest where
  fadd c (FOldest [] _) = FOldest [c] []
  fadd c (FOldest xs ys) = FOldest xs (c:ys)
  fremove _ (FOldest [] _) = FOldest [] []
  fremove _ (FOldest [_] ys) = FOldest (reverse ys) []
  fremove _ (FOldest (_:xs) ys) = FOldest xs ys
  fselect (FOldest [] _) = pure Nothing
  fselect (FOldest (x:_) _) = pure $ Just x
  
    
randomElt :: [a] -> IO (Maybe a)
randomElt [] = pure Nothing
randomElt xs = do
  i <- getPCGRandom $ randomR (0, length xs - 1)
  pure $ Just $ xs !! i

randomUnvisited :: Coord -> Grid -> IO (Maybe Coord)
randomUnvisited c g = randomElt $ unvisitedNodes c g

unvisitedNodes :: Coord -> Grid -> [Coord]
unvisitedNodes = adjacentNodesOf (== GridEmpty)

-- unvisitedNodesR :: Coord -> Grid -> IO [Coord]
-- unvisitedNodesR x g = shuffleM $ unvisitedNodes x g

emptyNodeR :: Grid -> IO (Maybe Coord)
emptyNodeR g = randomElt $ filter predicate $ nodeCoords g
  where
    predicate x = node x g == GridEmpty

mazify' :: (Frontier a) => Grid -> a -> IO Grid
mazify' grid front = do
  maybeCell <- fselect front
  case maybeCell of
    Nothing -> pure grid
    Just x -> do
      unvisited <- randomUnvisited x grid
      case unvisited of
        Nothing -> mazify' grid $ fremove x front
        Just n -> mazify' (carve grid x n) $ fadd n front

mazify :: MazeAlgo -> Grid -> IO Grid
mazify MazeRecBT grid = mazeGrid' (FRecBT []) grid
mazify MazePrim grid = mazeGrid' (FPrim []) grid
mazify MazeOldest grid = mazeGrid' (FOldest [] []) grid

mazeGrid :: MazeAlgo -> Coord -> Coord -> IO Grid
mazeGrid MazeRecBT = makeMazeGrid $ FRecBT []
mazeGrid MazePrim = makeMazeGrid $ FPrim []
mazeGrid MazeOldest = makeMazeGrid $ FOldest [] []

mazeGrid' :: (Frontier a) => a -> Grid -> IO Grid
mazeGrid' front grid = do
  xMaybe <- emptyNodeR grid
  case xMaybe of
    Nothing -> pure grid
    Just x -> do
      newGrid <- mazify' (visit grid x) (fadd x front)
      mazeGrid' front newGrid

makeMazeGrid :: (Frontier a) => a -> Coord -> Coord -> IO Grid
makeMazeGrid front gmin gmax = mazeGrid' front $ emptyUnlinkedGrid gmin gmax
