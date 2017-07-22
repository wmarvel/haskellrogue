module Main where

import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

data World = World
  { hero :: Coord
  }

data Command
  = North
  | NEast
  | East
  | SEast
  | South
  | SWest
  | West
  | NWest
  | Exit

main :: IO ()
main = do
  initDisplay
  gameLoop $ World (0, 0)

gameLoop :: World -> IO ()
gameLoop world = do
  renderWorld world
  command <- getCommand
  case command of
    Exit -> exitGame
    _ -> updateWorld world command

getCommand :: IO Command
getCommand = do
  char <- getChar
  case char of
    'k' -> return North
    'u' -> return NEast
    'l' -> return East
    'n' -> return SEast
    'j' -> return South
    'b' -> return SWest
    'h' -> return West
    'y' -> return NWest
    'q' -> return Exit
    _ -> getCommand

renderWorld :: World -> IO ()
-- renderWorld world@(World (x, y)) = do
renderWorld (World (x, y)) = do  
  clearScreen
  setCursorPosition y x
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
  putStr "@"

updateWorld :: World -> Command -> IO ()
updateWorld w@(World oldHero) command = gameLoop $ w {hero = newHero}
  where
    newHero = (x2, y2)
    (x1, y1) = oldHero |+| toDirection command
    hConst i = max 0 (min i 80)
    x2 = hConst x1
    y2 = hConst y1

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

resetDisplay :: IO ()
resetDisplay = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull White]

(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toDirection :: Command -> Coord
toDirection North = (0, -1)
toDirection NEast = (1, -1)
toDirection East = (1, 0)
toDirection SEast = (1, 1)
toDirection South = (0, 1)
toDirection SWest = (-1, 1)
toDirection West = (-1, 0)
toDirection NWest = (-1, -1)
toDirection _ = (0, 0)
