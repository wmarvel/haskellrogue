module Level.LevelGen where

import Coord.Types
import Data.Bits
import qualified Data.Set as S
import Level.Grid.GrowingTree
import Level.Grid.Types
import System.Random
import Types
import Level

data Room = Room
  { rPos :: Coord
  , rWidth :: Int
  , rHeight :: Int
  } deriving Show

-- Some useful random functions.
randElt :: [a] -> IO a
randElt xs = do
  index <- randInt 0 (length xs - 1)
  pure $ xs !! index
  
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

randomGrid :: Coord -> Coord -> Int -> IO Grid
randomGrid gmin gmax cnt = do
  rooms <- generateRooms gmin gmax grid cnt
  maze <- mazify MazePrim $ placeRooms grid $ rooms
  linked <- linkGrid maze rooms
  --pure $ linked
  pure $ fillDeadEnds linked
  where
    grid = emptyUnlinkedGrid gmin gmax

linkGrid :: Grid -> [Room] -> IO Grid
linkGrid grid [] = linkGrid' grid $ S.fromList $ nodeCoords grid
linkGrid grid (rm:rms) = do
  newGrid <- linkRoom grid rm
  linkGrid newGrid rms

linkRoom :: Grid -> Room -> IO Grid
linkRoom grid room = do
  recur <- randBool 0.5
  newGrid <- randomLink (roomEdge room) grid
  if recur
    then linkRoom newGrid room
    else pure newGrid
  
roomEdge :: Room -> CoordSet
roomEdge (Room (x, y) w h) = foldl (flip S.insert) S.empty coords
  where
    (x', y') = (x, y) |+| (w, h)
    coords =
      [ (x'', y'')
      | x'' <- [x .. x']
      , y'' <- [y .. y']
      , x'' == x || x'' == x' || y'' == y || y'' == y'
      ]

linkGrid' :: Grid -> CoordSet -> IO Grid
linkGrid' grid nodes = do
  linkedNodes <- reachableNodes grid
  if linkedNodes == nodes 
  then pure grid
  else do
    newGrid <- randomLink linkedNodes grid
    linkGrid' newGrid nodes

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
      case filter predicate $ unlinkedNeighbors x grid of
        [] -> xs
        cs' -> (x, cs') : xs
    predicate c = not $ S.member c cs

randomLink :: CoordSet -> Grid -> IO Grid
randomLink reachables grid =
  case linkablePairs reachables grid of
    [] -> do
      pure grid
    xs -> do
      (x, xs') <- randElt xs
      x' <- randElt xs'
      pure $ link' grid x x'

fillDeadEnds :: Grid -> Grid
fillDeadEnds grid =
  case deadEnds grid of
    [] -> grid
    ends -> fillDeadEnds $ fillPass ends
  where
    fillPass ends' = foldl fillEnd grid ends'
    fillEnd g c = case linkedNeighbors c g of
      [] -> setNode GridEdgeWall g c
      (c':_) -> fillEnd (unlink g c' c) c

deadEnds :: Grid -> [Coord]
deadEnds g = foldl buildList [] (nodeCoords g)
  where
    buildList l c =
      if isDeadEnd g c
      then c : l
      else l
    linkCount g'' c'' = length $ linkedNeighbors c'' g''
    isDeadEnd g' c' = (node c' g' == GridFloor) && (linkCount g' c' <= 1)

-- copy a grid into a level
copyFromGrid :: Level -> Grid -> IO Level
copyFromGrid level grid = pure $ foldl copyCell level $ levelCoords level
  where
    copyCell lvl x = updateTile x (cellToTile (cell x grid)) lvl
    cellToTile GridEmpty = Wall
    cellToTile GridFloor = Floor
    cellToTile GridEdgeWall = Wall
    cellToTile GridWallHard = Wall
    cellToTile GridEdgeDoor = Dr Closed

randomLevel :: Level -> IO Level
randomLevel lvl = do
  grid <- randomGrid gmin gmax 25
  rlvl <- copyFromGrid lvl grid
  placeStairs rlvl
  where
    toGrid (x, y) = (quot x 2, quot y 2)
    gmin = toGrid $ lMin lvl
    gmax = toGrid $ lMax lvl

placeStairs :: Level -> IO Level
placeStairs level = do
  level' <- placeUp level
  down <- randomSpawn level
  putStair down (St Down) level'
  where
    putStair coord stype lvl = pure $ updateTile coord stype lvl
    placeUp lvl =
      if lDepth lvl > 0
        then do
          up <- randomSpawn lvl
          putStair up (St Up) lvl
        else pure lvl
