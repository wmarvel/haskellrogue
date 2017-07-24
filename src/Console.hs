module Console where

import System.Console.ANSI

import Level
import Types

class ConsoleRenderable a where
  toRenderChar :: a -> Char
  toRenderSGR :: a -> [SGR]

instance ConsoleRenderable Hero where
  toRenderChar _ = '@'
  toRenderSGR _ =
    [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]

instance ConsoleRenderable Tile where
  toRenderChar tile =
    case tile of
      Acid -> '~'
      (Dr Closed) -> '+'
      (Dr Opened) -> '\''
      (St Down) -> '>'
      (St Up) -> '<'
      Wall -> '\x2588' --'#'
      Floor -> ' '
  toRenderSGR tile =
    case tile of
      Acid ->
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
      (Dr Closed) ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Magenta]
      (Dr Opened) ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Magenta]
      (St Down) ->
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
      (St Up) ->
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
      Wall ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull White]
      Floor ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Black]

render :: (ConsoleRenderable a) => Coord -> a -> IO ()
render coord x = do
  uncurry (flip setCursorPosition) coord
  setSGR $ toRenderSGR x
  putChar $ toRenderChar x

coordToTile :: World -> Coord -> Tile
coordToTile (World _ level) coord = lookupTile coord level

renderCoord :: World -> Coord -> IO ()
renderCoord world coord = do
  render coord $ coordToTile world coord

renderHero :: World -> IO ()
renderHero world@(World hero@(Hero curPos oldPos) _) = do
  render curPos hero
  if curPos == oldPos
    then return ()
    else renderCoord world oldPos

renderCoords :: World -> [Coord] -> IO ()
renderCoords world coords = mapM_ (renderCoord world) coords

renderWorld :: World -> IO ()
renderWorld world = do
  renderCoords world $ updatedCoords $ wLevel world
  renderHero world

