module Main where

import System.Console.ANSI
import System.IO

import Console
import Level
import Types

main :: IO ()
main = do
  initDisplay
  renderWorld world
  gameLoop world
  where world = makeWorld { wLevel = level1 }
  

gameLoop :: World -> IO ()
gameLoop world = do
  renderHero world
  command <- getCommand
  case command of
    Exit -> exitGame
    _ -> updateWorld world command

getCommand :: IO Command
getCommand = do
  char <- getChar
  case char of
    'k' -> return (Dir North)
    'u' -> return (Dir NEast)
    'l' -> return (Dir East)
    'n' -> return (Dir SEast)
    'j' -> return (Dir South)
    'b' -> return (Dir SWest)
    'h' -> return (Dir West)
    'y' -> return (Dir NWest)
    'q' -> return Exit
    _ -> getCommand

canOccupy :: Coord -> World -> Bool
canOccupy coord world = not $ isWall coord level || isClosedDoor coord level
  where level = wLevel world

updateWorld :: World -> Command -> IO ()
updateWorld world@(World oldHero _) command = gameLoop $ world {wHero = newHero}
  where
    newHero = oldHero {hCurPos = newPos, hOldPos = oldPos}
    oldPos = hCurPos oldHero
    newPos =
      if canOccupy (x, y) world
        then (x, y)
        else oldPos
    (x, y) = oldPos |+| toDirection command

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

toDirection :: Command -> Coord
toDirection (Dir North) = (0, -1)
toDirection (Dir NEast) = (1, -1)
toDirection (Dir East) = (1, 0)
toDirection (Dir SEast) = (1, 1)
toDirection (Dir South) = (0, 1)
toDirection (Dir SWest) = (-1, 1)
toDirection (Dir West) = (-1, 0)
toDirection (Dir NWest) = (-1, -1)
toDirection _ = (0, 0)
