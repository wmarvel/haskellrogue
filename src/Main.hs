module Main where

import Console
import Coord.Types
import Level
import Level.LevelGen
import System.Console.ANSI
import System.IO
import Types
import FOV

class CommandSource a where
  getCommand :: a -> IO Command

levelSize :: Coord
levelSize = (100, 50)

-- minimum number of rooms to attempt to place
minRooms :: Int
minRooms = 100

-- maximum number of rooms to attempt to place
maxRooms :: Int
maxRooms = 250

cleanLevel :: Level
cleanLevel = levelAllFloor levelSize

main :: IO ()
main = do
  rrooms <- randInt minRooms maxRooms
  rlevel <- randomLevel cleanLevel rrooms
  rspawn <- randomSpawn rlevel
  initDisplay
  startGame rlevel rspawn

spawnHero :: Coord -> Hero -> Hero
spawnHero coord hero = hero {hCurPos = coord, hOldPos = coord}

startGame :: Level -> Coord -> IO ()
startGame rlevel rspawn = gameLoop screen $ handleVisibles world
  where
    hero = spawnHero rspawn commoner
    world = makeWorld {wHero = hero, wLevel = rlevel}
    screen = initialScreen hero

gameLoop :: Screen -> World -> IO ()
gameLoop screen world = do
  newScreen <- renderWorld screen world
  command <- getCommand $ wHero world
  case command of
    Exit -> exitGame
    _ -> do
      newWorld <- updateWorld (unchangedWorld world) command
      gameLoop newScreen $ handleVisibles newWorld

instance CommandSource Hero where
  getCommand hero = do
    char <- getChar
    case char of
      c
        | c `elem` "wasdkulnjbhy" -> return $ Move $ getDirection c
      'o' -> do
        ochar <- getChar
        pure $ Operate $ getDirection ochar
      'q' -> pure Exit
      '>' -> pure $ TakeStair Down
      '<' -> pure $ TakeStair Up
      _ -> getCommand hero

canOccupy :: Coord -> World -> Bool
canOccupy coord world = not $ isWall coord level || isClosedDoor coord level
  where
    level = wLevel world

targetCoord :: Hero -> Direction -> Coord
targetCoord hero dir = hCurPos hero |+| toDirDelta dir

updateWorld :: World -> Command -> IO World
updateWorld world (Move dir) = pure $ world {wHero = moveHero world dir}
updateWorld world (Operate dir) = pure $ world {wLevel = opOn world dir}
updateWorld world (TakeStair stairs) = maybeTakeStairs stairs world
updateWorld world _ = pure world

maybeTakeStairs :: Stairs -> World -> IO World
maybeTakeStairs stairs world =
  if isStairs stairs coord level
    then do
      rrooms <- randInt minRooms maxRooms
      randLevel <- randomLevel (cleanLevel { lDepth = depth + delta }) rrooms
      rspawn <- randomSpawn randLevel
      clearScreen -- Need refactoring
      pure $ world { wHero = spawnHero rspawn hero, wLevel = randLevel }
    else pure world
  where
    hero = wHero world
    level = wLevel world
    coord = hCurPos $ wHero world
    depth = lDepth level
    delta =
      case stairs of
        Up -> -1
        Down -> 1
  
moveHero :: World -> Direction -> Hero
moveHero world@(World oldHero _) direction =
  oldHero {hCurPos = newPos, hOldPos = oldPos}
  where
    oldPos = hCurPos oldHero
    newPos =
      if canOccupy (x, y) world
        then (x, y)
        else oldPos
    (x, y) = targetCoord oldHero direction

fovRadius :: Int
fovRadius = 6

fovRays' :: RaySet
fovRays' = fovRays fovRadius

handleVisibles :: World -> World
handleVisibles world =
  world {wLevel = foldl (flip updateVisible) (wLevel world) herosees}
  where
    pos = hCurPos $ wHero world
    herosees = fov pos fovRays' $ wLevel world 

opOn :: World -> Direction -> Level
opOn world Stand = wLevel world
opOn (World hero level) dir =
  case targetCoord hero dir of
    target
      | isOpenDoor target level -> updateTile target (Dr Closed) level
      | isClosedDoor target level -> updateTile target (Dr Opened) level
    _ -> level

exitGame :: IO ()
exitGame = do
  resetDisplay
  putStrLn "Thank you for playing!"

initDisplay :: IO ()
initDisplay = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "hRogue"
  clearScreen

resetDisplay :: IO ()
resetDisplay = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]

toDirDelta :: Direction -> Coord
toDirDelta North = (0, -1)
toDirDelta NEast = (1, -1)
toDirDelta East = (1, 0)
toDirDelta SEast = (1, 1)
toDirDelta South = (0, 1)
toDirDelta SWest = (-1, 1)
toDirDelta West = (-1, 0)
toDirDelta NWest = (-1, -1)
toDirDelta Stand = (0, 0)

toDirection :: Command -> Coord
toDirection (Move dir) = toDirDelta dir
toDirection (Operate dir) = toDirDelta dir
toDirection _ = (0, 0)

getDirection :: Char -> Direction
getDirection 'w' = North
getDirection 'a' = West
getDirection 's' = South
getDirection 'd' = East
getDirection 'k' = North
getDirection 'u' = NEast
getDirection 'l' = East
getDirection 'n' = SEast
getDirection 'j' = South
getDirection 'b' = SWest
getDirection 'h' = West
getDirection 'y' = NWest
getDirection _ = Stand
