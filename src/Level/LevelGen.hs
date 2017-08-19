module Level.LevelGen where

import Coord.Types
import Data.Bits
import qualified Data.Set as S
import Level.Grid.GrowingTree
import Level.Grid.Types
import System.Random

data Room = Room
  { rPos :: Coord
  , rWidth :: Int
  , rHeight :: Int
  }

-- Some useful random functions.
randInt :: Int -> Int -> IO Int
randInt rMin rMax = getStdRandom $ randomR (rMin, rMax)

randMapped :: (Int -> Int) -> (Int -> Int) -> Int -> IO Int
randMapped fRes fLim limit = fmap fRes $ randInt 0 $ fLim limit

randOdd :: Int -> Int -> IO Int
randOdd x x' = fmap (+ x) $ randOdd' (x' - x)
  where
    randOdd' = randMapped (.|. 1) (.|. 1) . (subtract 1)

randEven :: Int -> Int -> IO Int
randEven x x' = fmap (+ x) $ randEven' (x' - x)
  where
    randEven' = randMapped (.&. (maxBound - 1)) (.|. 1)

randBool :: Double -> IO Bool
randBool chance = fmap (<=chance) (getStdRandom $ randomR (0.0, 1.0))

randNode :: Grid -> IO Coord
randNode grid = do
  rindex <- randInt 0 $ length nodes
  pure $ nodes !! rindex
  where nodes = nodeCoords grid

randRoom :: Coord -> Coord -> Coord -> IO Room
randRoom (dMin, dMax) (xMin, yMin) (xMax, yMax) = do
  wRand <- randEven dMin dMax
  hRand <- randEven dMin (quot dMax 2)
  randCol <- randOdd xMin $ xMax - wRand
  randRow <- randOdd yMin $ yMax - hRand
  pure $ Room {rPos = (randCol, randRow), rWidth = wRand, rHeight = hRand}

roomWithinGrid :: Grid -> Room -> Bool
roomWithinGrid g r =
  xMin <= x && yMin <= y && x' <= xMax && y' <= yMax
  where
    (x, y) = rPos r
    (x', y') = (x, y) |+| (rWidth r, rHeight r)
    (xMin, yMin) = gMin g
    (xMax, yMax) = gMax g

placeRoom :: Grid -> Room -> Grid
placeRoom g r = foldl setCell' g $ allCellCoords rg
  where
    setCell' = (setCell GridFloor)
    rg = emptyLinkedGrid pos (pos |+| dim)
    pos = rPos r
    dim = (rWidth r, rHeight r)

expandRoom :: Room -> Room
expandRoom (Room (x, y) w h) = Room (x - 1, y - 1) (w + 1) (h + 1)

roomsCollide :: Room -> Room -> Bool
roomsCollide (Room (x, y) width height) (Room (x', y') width' height') =
  x < x' + width' && x + width > x' && y < y' + height' && y + height > y'

anyRoomsCollide :: Room -> [Room] -> Bool
anyRoomsCollide room rooms = foldl roomsCollide' False rooms
  where
    roomsCollide' value room' = value || roomsCollide (expandRoom room) (expandRoom room')

generateRooms :: Coord -> Coord -> Grid -> Int -> IO [Room]
generateRooms cmin cmax grid cnt = foldl passCollision (pure []) [0 .. cnt]
  where
    passCollision iorooms _ = do
      rooms <- iorooms
      room <- randRoom (2, 10) cmin cmax
      if acceptableRoom room rooms
        then pure $ room : rooms
        else pure rooms
    acceptableRoom room' rooms' =
      roomWithinGrid grid room' && not (anyRoomsCollide room' rooms')

placeRooms :: Grid -> [Room] -> Grid
placeRooms grid rooms = foldl placeRoom grid rooms

-- FIXME : need all the floodfills to start within the rooms
unlinkedRoomMaze :: Coord -> Coord -> Int -> IO Grid
unlinkedRoomMaze gmin gmax cnt = do
  rooms <- generateRooms gmin gmax grid cnt
  mazify MazePrim $ placeRooms grid $ rooms
  where
    grid = emptyUnlinkedGrid gmin gmax

relinkGrid :: Grid -> IO Grid
relinkGrid grid = relinkGrid' grid $ S.fromList $ nodeCoords grid

relinkGrid' :: Grid -> CoordSet -> IO Grid
relinkGrid' grid nodes = do
  linkedNodes <- reachableNodes grid
  if linkedNodes == nodes 
  then pure grid
  else do
    newGrid <- randomRelink linkedNodes grid
    relinkGrid' newGrid nodes

reachableNodes :: Grid -> IO CoordSet
reachableNodes grid = do
  start <- randNode grid
  pure $ reachableNodes' start $ S.singleton start
  where
    reachableNodes' coord found = foldl maybeAdd found $ adjacentNodes coord
      where
        maybeAdd found' coord' =
          if S.member coord' found'
            then found'
            else if isLinked coord coord' grid
                   then reachableNodes' coord' $ S.insert coord' found'
                   else found'

linkablePairs :: CoordSet -> Grid -> [(Coord, [Coord])]
linkablePairs cs grid = foldl addLinkPair [] cs
  where
    addLinkPair xs x =
      case filter (flip S.member cs) $ unlinkedNeighbors x grid of
        [] -> xs
        cs' -> (x, cs') : xs

testIt :: IO [(Coord, [Coord])]
testIt = do
  grid <- unlinkedRoomMaze (0, 0) (30, 15) 5
  reachables <- reachableNodes grid
  pure $ linkablePairs reachables grid
  

randomRelink :: CoordSet -> Grid -> IO Grid
randomRelink reachables grid =
  case linkablePairs reachables grid of
    [] -> pure grid
    xs -> undefined


    

