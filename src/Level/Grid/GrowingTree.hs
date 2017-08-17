{-# LANGUAGE FlexibleInstances #-}
module Level.Grid.GrowingTree where

import Coord.Types
import Level.Grid.Types
import System.Random
import System.Random.Shuffle

data MazeAlgo
  = Backtrack

class (Eq a) => Frontier a where
  fmake :: a -> a
  fempty :: a
  fadd :: Coord -> a -> a
  fremove :: Coord -> a -> a
  fselect :: a -> IO (Maybe Coord)

instance Frontier [Coord] where
  fmake x = x
  fempty = []
  fadd = (:)
  fremove c l =
    case l of
      [] -> []
      (c':cs) ->
        if c == c'
          then cs
          else c' : fremove c cs
  fselect [] = pure Nothing
  fselect xs = pure $ Just $ head xs

randomElt :: [a] -> IO (Maybe a)
randomElt [] = pure Nothing
randomElt xs = do
  i <- getStdRandom $ randomR (0, length xs - 1)
  pure $ Just $ xs !! i

adjacentNodesOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
adjacentNodesOf f x g = filter predicate (adjacentNodes x)
  where
    predicate x' = isNodeInBounds g x' && (f $ node x' g)

unvisitedNodes :: Coord -> Grid -> [Coord]
unvisitedNodes x g = adjacentNodesOf (== GridEmpty) x g

unvisitedNodesR :: Coord -> Grid -> IO [Coord]
unvisitedNodesR x g = shuffleM $ unvisitedNodes x g

emptyNodeR :: Grid -> IO (Maybe Coord)
emptyNodeR g = randomElt $ filter predicate $ nodeCoords g
  where
    predicate x = (node x g) == GridEmpty

mazify :: (Frontier a) => Grid -> a -> IO Grid
mazify grid front = do
  maybeCell <- fselect front
  case maybeCell of
    Nothing -> pure grid
    Just x -> do
      unvisited <- unvisitedNodesR x grid
      case unvisited of
        [] -> mazify grid $ fremove x front
        (n:_) -> mazify (carve grid x n) $ fadd n front

mazeGrid :: MazeAlgo -> Coord -> Coord -> IO Grid
mazeGrid Backtrack = (mazeGrid'::[Coord]->Coord->Coord->IO Grid) []

mazeGrid' :: (Frontier a) => a -> Coord -> Coord -> IO Grid
mazeGrid' front gmin gmax = do
  xMaybe <- emptyNodeR grid
  case xMaybe of
    Nothing -> pure grid
    Just x -> do
      mazify (visit grid x) $ fadd x front
  where
    grid = emptyUnlinkedGrid gmin gmax
