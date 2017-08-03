module LevelGen where
import Data.Bits
import System.Random
import Types
import Level
import qualified Data.Set as S

data ConnectInfo = ConnectInfo
  { ciLevel :: Level
  , ciConnected :: S.Set Coord
  , ciConnectors :: S.Set Coord
  , ciToVisit :: [Coord]
  }

data Room = Room
  { rPos :: Coord
  , rWidth :: Int
  , rHeight :: Int
  }

data GenContext = GenContext
  { cRooms   :: Int -- The count of rooms to attempt to place
  , cRoomMinDim :: Int -- The minimum dimension of a room (width or height)
  , cRoomMaxDim :: Int -- The maximum dimension of a room (width or height)
  }

defaultContext :: GenContext
defaultContext = GenContext {cRooms = 25, cRoomMinDim = 6, cRoomMaxDim = 12}

emptyConnectInfo :: ConnectInfo
emptyConnectInfo =
  ConnectInfo
  { ciLevel = emptyLevel
  , ciConnected = S.empty
  , ciConnectors = S.empty
  , ciToVisit = []
  }

-- Some useful random functions.

randInt :: Int -> Int -> IO Int
randInt rMin rMax = getStdRandom $ randomR (rMin, rMax)

randRoom :: GenContext -> Coord -> IO Room
randRoom (GenContext _ dMin dMax) (xMax, yMax) = do
  randWidth <- randEven dMin dMax
  randHeight <- randEven dMin dMax
  randCol <- randOdd 0 $ xMax - randWidth
  randRow <- randOdd 0 $ yMax - randHeight
  pure $
    Room
    {rPos = (randCol, randRow), rWidth = randWidth, rHeight = randHeight}

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

randBool :: IO Bool
randBool = getStdRandom $ randomR (False, True)

-- Generate a perfect maze - currently uses recursive division.
insertWall :: [Coord] -> Level -> IO Level
insertWall coords level = pure $ foldr markWall level coords
  where markWall coord = updateTile coord Wall

vertCoords :: Int -> Int -> Int -> IO [Coord]
vertCoords column rMin rMax = do
  skipVal <- randEven rMin rMax
  pure $ [(column, row) | row <- [rMin .. rMax], row /= skipVal]

horzCoords :: Int -> Int -> Int -> IO [Coord]
horzCoords row cMin cMax = do
  skipVal <- randEven cMin cMax
  pure $ [(column, row) | column <- [cMin .. cMax], column /= skipVal]

undividable :: Level -> Bool
undividable level = x' <= x || y' <= y
  where
    (x, y) = lMin level
    (x', y') = lMax level

mazifyLevel :: Level -> IO Level
mazifyLevel level =
  if undividable level
    then pure level
    else do
      (part1, part2) <- divideLevel level
      mazified1 <- mazifyLevel part1
      mazified2 <- mazifyLevel part2
      pure $ joinLevels mazified1 mazified2

chooseHorizontal :: Level -> IO Bool
chooseHorizontal level =
  if levWidth == levHeight
    then randBool
    else if levWidth < levHeight
           then pure True
           else pure False
  where
    (x, y) = lMin level
    (x', y') = lMax level
    levWidth = x' - x
    levHeight = y' - y

divideLevel :: Level -> IO (Level, Level)
divideLevel level = do
  maybeHorizontal <- chooseHorizontal level
  if maybeHorizontal
    then divideHorizontal level
    else divideVertical level

divideHorizontal :: Level -> IO (Level, Level)
divideHorizontal level = do
  divRow <- randOdd rowMin rowMax
  wallCoords <- horzCoords divRow colMin colMax
  walled <- insertWall wallCoords level
  pure (part1 walled divRow, part2 walled divRow)
  where
    part1 walled divRow = walled { lMax = (colMax, divRow - 1) }
    part2 walled divRow = walled { lMin = (colMin, divRow + 1) }
    (colMin, rowMin) = lMin level
    (colMax, rowMax) = lMax level

divideVertical :: Level -> IO (Level, Level)
divideVertical level = do
  divCol <- randOdd colMin colMax
  wallCoords <- vertCoords divCol rowMin rowMax
  walled <- insertWall wallCoords level
  pure (part1 walled divCol, part2 walled divCol)
  where
    part1 walled divCol = walled { lMax = (divCol - 1, rowMax) }
    part2 walled divCol = walled { lMin = (divCol + 1, rowMin) }
    (colMin, rowMin) = lMin level
    (colMax, rowMax) = lMax level

-- Now that we can generate a perfect maze, drop some rooms in it
randomLevel :: Level -> IO Level
randomLevel level = do
  maze <- mazifyLevel level
  placed <- placeRooms maze $ generateRooms defaultContext $ lMax maze
  connected <- reconnectLevel defaultContext placed
  pure $ fillDeadEnds connected

roomsCollide :: Room -> Room -> Bool
roomsCollide (Room (x, y) width height) (Room (x', y') width' height') =
  x < x' + width' && x + width > x' && y < y' + height' && y + height > y'

anyRoomsCollide :: Room -> [Room] -> Bool
anyRoomsCollide room rooms = foldl roomsCollide' False rooms
  where
    roomsCollide' value room' = value || roomsCollide room room'

generateRooms :: GenContext -> Coord -> IO [Room]
generateRooms ctx coord = foldl passCollision (pure []) [0 .. cRooms ctx]
  where
    passCollision iorooms _ = do
      rooms <- iorooms
      room <- randRoom ctx coord
      if anyRoomsCollide room rooms
        then iorooms
        else pure $ room : rooms

placeRoom :: Level -> Room -> Level
placeRoom level (Room (x, y) width height) =
  foldl putRoomTile level [(x', y') | x' <- [x .. xMax], y' <- [y .. yMax]]
  where
    xMax = x + width
    yMax = y + height
    getRoomTile (col, row) =
      if col == x || col == xMax || row == y || row == yMax
        then Floor -- Wall -- Wall when reconnection is complete
        else Floor
    putRoomTile level' coord = updateTile coord (getRoomTile coord) level'

placeRooms :: Level -> IO [Room] -> IO Level
placeRooms level iorooms = do
  rooms <- iorooms
  pure $ foldl placeRoom level rooms

crossAdjacents :: Coord -> [Coord]
crossAdjacents (col, row) =
  [(col - 1, row), (col + 1, row), (col, row - 1), (col, row + 1)]

allAdjacents :: Coord -> [Coord]
allAdjacents (row, col) =
  [ (x, y)
  | x <- [col - 1 .. col + 1]
  , y <- [row - 1 .. row + 1]
  , (x, y) /= (col, row)
  ]

isDeadEnd :: Coord -> Level -> Bool
isDeadEnd coord level = isFloor coord level && adjacentWalls >= 3
  where
    adjacentWalls = length $ filter (== True) $ map isWall' coords
    coords = crossAdjacents coord
    isWall' = flip isWall level

deadEnds :: Level -> [Coord]
deadEnds level = foldl buildCoords [] $ levelCoords level
  where
    isDeadEnd' = flip isDeadEnd level
    buildCoords list coord =
      if isDeadEnd' coord
        then coord : list
        else list

fillDeadEnds :: Level -> Level
fillDeadEnds level = case deadEnds level of
  [] -> level
  deads -> fillDeadEnds $ onePass deads
  where
    fillDeadEnd level' coord = updateTile coord Wall level'
    onePass ends = foldl fillDeadEnd level ends

fullyConnected :: ConnectInfo -> Bool
fullyConnected ci = ciConnected ci == occupiableCoords lvl
  where lvl = ciLevel ci

reconnectLevel :: GenContext -> Level -> IO Level
reconnectLevel _ level = do
  seed <- randomSpawn level
  reconnectLevel' $ emptyConnectInfo {ciLevel = level, ciToVisit = [seed]}
  where
    reconnectLevel' ci =
      if fullyConnected ci
        then pure $ ciLevel ci
        else pure $ ciLevel ci




  


