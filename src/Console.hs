module Console where

import System.Console.ANSI

import Level
import Types

coordToChar :: Coord -> World -> Char
coordToChar coord (World hero level)
  | hCurPos hero == coord = '@'
  | isAcid coord level = '~'
  | isClosedDoor coord level = '+'
  | isOpenDoor coord level = '\''
  | isDownStairs coord level = '>'
  | isUpStairs coord level = '<'
  | isGold coord level = '$'
  | isPotion coord level = '!'
  | isWeapon coord level = '|'
  | isVillian coord level = 'v'
  | isWall coord level = '#'
  | otherwise = ' '

drawChar '@' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  putChar '@'
drawChar '#' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar  '#'
drawChar '!' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Magenta]
  putChar '!'
drawChar '$' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Yellow ]
  putChar '$'
drawChar 'v' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putChar 'v'
drawChar '|' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Cyan ]
  putChar '|'
drawChar '>' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar '>'
drawChar '<' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar '<'
drawChar '\n' = do
  putChar '\n'
drawChar '+' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Magenta ]
  putChar '+'
drawChar '\'' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Magenta ]
  putChar '\''
drawChar '~' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  putChar '~'  
drawChar _ = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar ' '


drawCoord :: World -> Coord -> IO ()
drawCoord world coord = do
  uncurry (flip setCursorPosition) coord
  drawChar (coordToChar coord world)

renderHero :: World -> IO ()
renderHero world@(World ourHero _)
  | curPos == oldPos = return ()
  | otherwise = do
    drawCoord world oldPos
    drawCoord world curPos
  where
    curPos = hCurPos ourHero
    oldPos = hOldPos ourHero

renderWorld :: World -> IO ()
renderWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    (x', y') = lMax (wLevel world)
    chars = [[coordToChar (x, y) world | x <- [0 .. x']] | y <- [0 .. y']]


-- renderWorld :: World -> IO ()
-- renderWorld (World (x, y)) = do  
--   clearScreen
--   setCursorPosition y x
--   setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
--   putStr "@"
            

