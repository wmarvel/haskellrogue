module Level.Grid.GrowingTree where

import Level.Grid.Types
import System.Random
import System.Random.Shuffle

randomElt :: [a] -> IO (Maybe a)
randomElt [] = pure Nothing
randomElt xs = do
  i <- getStdRandom $ randomR (0, length xs - 1)
  pure $ Just $ xs !! i


adjacentNodesOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
adjacentNodesOf f x g = filter predicate (adjacentNodes x)
  where predicate x' = isNodeInBounds g x' && (f $ node x' g)

unvisitedNodes :: Coord -> Grid -> [Coord]
unvisitedNodes x g = adjacentNodesOf (==GridEmpty) x g

unvisitedNodesR :: Coord -> Grid -> IO [Coord]
unvisitedNodesR x g = shuffleM $ unvisitedNodes x g

emptyNodeR :: Grid -> IO (Maybe Coord)
emptyNodeR g = randomElt $ filter predicate $ nodeCoords g
  where predicate x = (node x g) == GridEmpty

mazify :: Grid -> [Coord] -> IO Grid
mazify g [] = pure g
mazify g (x:xs) = do
  unvisited <- unvisitedNodesR x g
  case unvisited of
    [] -> mazify g xs
    (n : _) -> mazify (link (visit g n) x n) (n : x : xs)
  
mazeGrid :: IO Grid
mazeGrid = do
  x' <- emptyNodeR grid
  case x' of
    Nothing -> pure grid
    Just x -> do
      mazify (visit grid x) [x]
  where grid = emptyUnlinkedGrid (0, 0) (10, 5)
