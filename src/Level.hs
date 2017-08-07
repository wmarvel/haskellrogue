module Level where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Random
import Types
import Coord.Types

sizedLevel :: Coord -> Level
sizedLevel bound = emptyLevel {lMax = bound}

stringsToLevel :: [String] -> Level
stringsToLevel str = foldl populate emptyLevel {lMax = coordMax} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords = [[(x, y) | x <- [0 ..]] | y <- [0 ..]]
    dimMax f f' = maximum . map (f . f') $ asciiMap
    coordMax = (dimMax fst fst, dimMax snd fst)
    populate lvl (coord, tile) =
      case tile of
        '#' -> updateTile coord Wall lvl
        '<' -> updateTile coord (St Up) lvl
        '>' -> updateTile coord (St Down) lvl
        '+' -> updateTile coord (Dr Closed) lvl
        '\'' -> updateTile coord (Dr Opened) lvl
        '.' -> updateTile coord Floor lvl
        _ -> lvl

isAtCoord :: (a -> Bool) -> Bool -> Coord -> M.Map Coord a -> Bool
isAtCoord f defval coord valuemap =
  case fmap f $ M.lookup coord valuemap of
    Nothing -> defval
    Just value -> value

isTile :: (Tile -> Bool) -> Bool -> Coord -> Level -> Bool
isTile f defval coord level = isAtCoord f defval coord (lTiles level) 

isClosedDoor :: Coord -> Level -> Bool
isClosedDoor = isTile (==(Dr Closed)) False

isOpenDoor :: Coord -> Level -> Bool
isOpenDoor = isTile (==(Dr Opened)) False

isWall :: Coord -> Level -> Bool
isWall = isTile (==Wall) True

isFloor :: Coord -> Level -> Bool
isFloor = isTile (==Floor) False

isDownStairs :: Coord -> Level -> Bool
isDownStairs = isTile (==(St Down)) False

isUpStairs :: Coord -> Level -> Bool
isUpStairs = isTile (==(St Up)) False

isOccupiable :: Coord -> Level -> Bool
isOccupiable coord = not . isWall coord

map1 :: [String]
map1 =
  [ "##############"
  , "#............#          ######"
  , "#............############....#"
  , "#............\'..........+....#"
  , "#............############....#"
  , "#............#          #....#"
  , "#............#          #.>..#"
  , "##############          ######"
  ]

level1 :: Level
level1 = stringsToLevel map1

levelAllFloor :: Coord -> Level
levelAllFloor limit@(xMax, yMax) = foldl putFloorTile newLevel coords
  where
    newLevel = sizedLevel limit
    coords = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    putFloorTile level coord = updateTile coord Floor level

lookupTile :: Coord -> Level -> Tile
lookupTile coord level = M.findWithDefault Wall coord $ lTiles level
  
updateTile :: Coord -> Tile -> Level -> Level
updateTile coord tile level = level { lTiles = tiles, lChanged = changed }
  where
    tiles = case tile of
      Wall -> M.delete coord $ lTiles level
      _ -> M.insert coord tile $ lTiles level
    changed = S.insert coord $ lChanged level

updateSeen :: Coord -> Level -> Level
updateSeen coord level =
  if S.member coord $ lSeen level
    then level
    else level {lChanged = changed, lSeen = seen}
  where
    changed = S.insert coord $ lChanged level
    seen = S.insert coord $ lSeen level

updatedCoords :: Level -> [Coord]
updatedCoords = S.toList . lChanged 

unchangedWorld :: World -> World
unchangedWorld world@(World _ level) =
  world { wLevel = level { lChanged = S.empty }}

changedWorld :: World -> World
changedWorld world@(World _ level) =
  world { wLevel = M.foldrWithKey updateTile level $ lTiles level }

copyTiles :: Coord -> Coord -> Level -> Level -> Level
copyTiles (x1, y1) (x2, y2) source target =
  foldr copyTile target [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
  where
    copyTile coord = updateTile coord (lookupTile coord source)

joinLevels :: Level -> Level -> Level
joinLevels l1 l2 =
  merged {lMin = minCoord l1Min l2Min, lMax = maxCoord l1Max l2Max}
  where
    l1Min = lMin l1 
    l1Max = lMax l1 
    l2Min = lMin l2
    l2Max = lMax l2
    merged = copyTiles l2Min l2Max l2 l1

randomSpawn :: Level -> IO Coord
randomSpawn level = do
  offset <- getStdRandom $ randomR (0, length floors - 1)
  pure $ floors !! offset
  where
    okTile = flip isFloor level
    floors = filter okTile coords
    coords = M.keys $ lTiles level

levelCoords :: Level -> [Coord]
levelCoords = M.keys . lTiles

occupiableCoords :: Level -> S.Set Coord
occupiableCoords level = S.fromList $ filter isOccupiable' $ levelCoords level
  where isOccupiable' = flip isOccupiable level
