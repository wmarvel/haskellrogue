module Level.Grid.GrowingTree where

import Coord.Types
import Level.Grid.Types
import System.Random
import System.Random.Shuffle

-- Using FRecBT as the frontier results in a recursive backtracker
data FRecBT = FRecBT [Coord]

-- Using FPrim as the frontier results in Prim's algorithm
data FPrim = FPrim [Coord]

data MazeAlgo = MazeRecBT | MazePrim

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
    cs' <- shuffleM cs
    pure $ Just $ head cs'

randomElt :: [a] -> IO (Maybe a)
randomElt [] = pure Nothing
randomElt xs = do
  i <- getStdRandom $ randomR (0, length xs - 1)
  pure $ Just $ xs !! i

unvisitedNodes :: Coord -> Grid -> [Coord]
unvisitedNodes x g = adjacentNodesOf (== GridEmpty) x g

unvisitedNodesR :: Coord -> Grid -> IO [Coord]
unvisitedNodesR x g = shuffleM $ unvisitedNodes x g

emptyNodeR :: Grid -> IO (Maybe Coord)
emptyNodeR g = randomElt $ filter predicate $ nodeCoords g
  where
    predicate x = (node x g) == GridEmpty

mazify' :: (Frontier a) => Grid -> a -> IO Grid
mazify' grid front = do
  maybeCell <- fselect front
  case maybeCell of
    Nothing -> pure grid
    Just x -> do
      unvisited <- unvisitedNodesR x grid
      case unvisited of
        [] -> mazify' grid $ fremove x front
        (n:_) -> mazify' (carve grid x n) $ fadd n front

mazify :: MazeAlgo -> Grid -> IO Grid
mazify MazeRecBT grid = mazeGrid' (FRecBT []) grid
mazify MazePrim grid = mazeGrid' (FPrim []) grid

mazeGrid :: MazeAlgo -> Coord -> Coord -> IO Grid
mazeGrid MazeRecBT = makeMazeGrid $ FRecBT []
mazeGrid MazePrim = makeMazeGrid $ FPrim []

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
