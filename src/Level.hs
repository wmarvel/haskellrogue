module Level where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

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
        '~' -> updateTile coord Acid lvl
        _ -> lvl

isAtCoord :: (a -> Bool) -> Coord -> M.Map Coord a -> Bool
isAtCoord f coord valuemap =
  case fmap f $ M.lookup coord valuemap of
    Nothing -> False
    Just value -> value

isTile :: (Tile -> Bool) -> Coord -> Level -> Bool
isTile f coord level = isAtCoord f coord (lTiles level)

isAcid :: Coord -> Level -> Bool
isAcid = isTile (==Acid)

isClosedDoor :: Coord -> Level -> Bool
isClosedDoor = isTile (==(Dr Closed))

isOpenDoor :: Coord -> Level -> Bool
isOpenDoor = isTile (==(Dr Opened))

isWall :: Coord -> Level -> Bool
isWall = isTile (==Wall)

isDownStairs :: Coord -> Level -> Bool
isDownStairs = isTile (==(St Down))

isUpStairs :: Coord -> Level -> Bool
isUpStairs = isTile (==(St Up))


map1 :: [String]
map1 =
  [ "##############"
  , "#            #          ######"
  , "#            ############    #"
  , "#            \'          +    #"
  , "#    ~~      ############    #"
  , "#     ~~     #          #    #"
  , "#      ~~    #          # >  #"
  , "##############          ######"
  ]
            

level1 :: Level
level1 = stringsToLevel map1

lookupTile :: Coord -> Level -> Tile
lookupTile coord level = case M.lookup coord $ lTiles level of
  Just tile -> tile
  Nothing -> Floor
  
updateTile :: Coord -> Tile -> Level -> Level
updateTile coord tile level = level { lTiles = tiles, lChanged = changed }
  where
    tiles = M.insert coord tile $ lTiles level
    changed = S.insert coord $ lChanged level

updatedCoords :: Level -> [Coord]
updatedCoords = S.toList . lChanged 


unchangedWorld :: World -> World
unchangedWorld world@(World _ level) =
  world { wLevel = level { lChanged = S.empty }}

changedWorld :: World -> World
changedWorld world@(World _ level) =
  world { wLevel = M.foldrWithKey updateTile level $ lTiles level }
