module LevelGen where
import Data.Bits
import System.Random
import Types
import Level

-- Some useful random functions.
randMapped :: (Int -> Int) -> (Int -> Int) -> Int -> IO Int
randMapped fRes fLim limit = fmap fRes $ getStdRandom $ randomR (0, fLim limit)

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
  putStrLn $ show skipVal
  pure $ [(row, column) | row <- [rMin .. rMax], row /= skipVal]

horzCoords :: Int -> Int -> Int -> IO [Coord]
horzCoords row cMin cMax = do
  skipVal <- randEven cMin cMax
  putStrLn $ show skipVal
  pure $ [(row, column) | column <- [cMin .. cMax], column /= skipVal]

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
           then pure False
           else pure True
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
    part1 walled divRow = walled { lMax = (divRow - 1, colMax) }
    part2 walled divRow = walled { lMin = (divRow + 1, colMin) }
    (rowMin, colMin) = lMin level
    (rowMax, colMax) = lMax level

divideVertical :: Level -> IO (Level, Level)
divideVertical level = do
  divCol <- randOdd colMin colMax
  wallCoords <- vertCoords divCol rowMin rowMax
  walled <- insertWall wallCoords level
  pure (part1 walled divCol, part2 walled divCol)
  where
    part1 walled divCol = walled { lMax = (rowMax, divCol - 1) }
    part2 walled divCol = walled { lMin = (rowMin, divCol + 1) }
    (rowMin, colMin) = lMin level
    (rowMax, colMax) = lMax level

  





  

