module Level where

import qualified Data.Map as M
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
        '#' -> lvl {lTiles = M.insert coord Wall oldTiles}
        '<' -> lvl {lTiles = M.insert coord (St Up) oldTiles}
        '>' -> lvl {lTiles = M.insert coord (St Down) oldTiles}
        '+' -> lvl {lTiles = M.insert coord (Dr Closed) oldTiles}
        '\'' -> lvl {lTiles = M.insert coord (Dr Opened) oldTiles}
        '~' -> lvl {lTiles = M.insert coord Acid oldTiles}
        _ -> lvl
        where oldTiles = lTiles lvl



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
updateTile coord tile level = level { lTiles = newTiles }
  where newTiles = M.insert coord tile (lTiles level)




