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

renderHero :: Hero -> IO ()
renderHero hero@(Hero coord _) = render coord hero

renderWorld :: World -> IO ()
renderWorld world = do
  mapM_ (renderCoord world) coords
  renderHero (wHero world)
  where
    (x', y') = lMax (wLevel world)
    coords = [(x, y) | x <- [0 .. x'], y <- [0 .. y']]
  
