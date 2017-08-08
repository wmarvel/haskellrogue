module Main where

import Console
import Coord.Types
import Level
import LevelGen
import System.Console.ANSI
import System.IO
import Types
import FOV

main :: IO ()
main = do
  rlevel <- randomLevel $ levelAllFloor (100, 100)
  rspawn <- randomSpawn rlevel
  initDisplay
  gameLoop screen $
    handleSeen $
    makeWorld {wHero = commoner {hCurPos = rspawn}, wLevel = rlevel}
  where
    screen = (Screen (0, 0) (79, 24) True)

gameLoop :: Screen -> World -> IO ()
gameLoop screen world = do
  newScreen <- renderWorld screen world
  command <- getCommand
  case command of
    Exit -> exitGame
    _ ->
      gameLoop newScreen $
      handleSeen $ updateWorld (unchangedWorld world) command

getCommand :: IO Command
getCommand = do
  char <- getChar
  case char of
    c
      | c `elem` "wasdkulnjbhy" -> return $ Move $ getDirection c
    'o' -> do
      ochar <- getChar
      return $ Operate $ getDirection ochar
    'q' -> return Exit
    _ -> getCommand

canOccupy :: Coord -> World -> Bool
canOccupy coord world = not $ isWall coord level || isClosedDoor coord level
  where
    level = wLevel world

targetCoord :: Hero -> Direction -> Coord
targetCoord hero dir = hCurPos hero |+| toDirDelta dir

updateWorld :: World -> Command -> World
updateWorld world (Move dir) = world {wHero = moveHero world dir}
updateWorld world (Operate dir) = world {wLevel = opOn world dir}
updateWorld world _ = world

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

fovRays' :: [[Coord]]
fovRays' = fovRays 12

handleSeen :: World -> World
handleSeen world =
  world {wLevel = foldl (flip updateSeen) (wLevel world) herosees}
  where
    pos = hCurPos $ wHero world
    herosees = fov pos fovRays' $ wLevel world 

opOn :: World -> Direction -> Level
opOn world Stand = wLevel world
opOn (World hero level) dir =
  case (targetCoord hero dir) of
    target
      | isOpenDoor target level == True -> updateTile target (Dr Closed) level
      | isClosedDoor target level == True -> updateTile target (Dr Opened) level
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
