module Main where

import System.Console.ANSI
import System.IO

import Console
import Level
import Types

main :: IO ()
main = do
  initDisplay
  fullRenderWorld world
  gameLoop world
  where world = makeWorld { wLevel = level1 }

gameLoop :: World -> IO ()
gameLoop world = do
  fastRenderWorld world
  command <- getCommand
  case command of
    Exit -> exitGame
    _ -> updateWorld (unchangedWorld world) command

getCommand :: IO Command
getCommand = do
  char <- getChar
  case char of
    c | c `elem` "kulnjbhy" -> return $ Move $ getDirection c
    'o' -> do
      ochar <- getChar
      return $ Operate $ getDirection ochar
    'q' -> return Exit
    _ -> getCommand

canOccupy :: Coord -> World -> Bool
canOccupy coord world = not $ isWall coord level || isClosedDoor coord level
  where level = wLevel world

targetCoord :: Hero -> Direction -> Coord
targetCoord hero dir = hCurPos hero |+| toDirDelta dir

updateWorld :: World -> Command -> IO ()
updateWorld world (Move dir) = gameLoop $ world {wHero = moveHero world dir}
updateWorld world (Operate dir) = gameLoop $ world {wLevel = opOn world dir}
updateWorld world _ = gameLoop $ world
  
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
  setTitle "HaskellRogue"
  clearScreen

resetDisplay :: IO ()
resetDisplay = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]

(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
getDirection 'k' = North
getDirection 'u' = NEast
getDirection 'l' = East
getDirection 'n' = SEast
getDirection 'j' = South
getDirection 'b' = SWest
getDirection 'h' = West
getDirection 'y' = NWest
getDirection _ = Stand
