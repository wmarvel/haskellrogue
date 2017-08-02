module Console where

import Level
import Types
import System.Console.ANSI

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
      (Dr Closed) -> '+'
      (Dr Opened) -> '\''
      (St Down) -> '>'
      (St Up) -> '<'
      Wall -> '\x2588' --'#'
      Floor -> '.'
  toRenderSGR tile =
    case tile of
      (Dr Closed) ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Magenta]
      (Dr Opened) ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Magenta]
      (St Down) ->
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
      (St Up) ->
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
      Wall ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
      Floor ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull White]

render :: (ConsoleRenderable a) => Screen -> Coord -> a -> IO ()
render screen coord x = do
  uncurry (flip setCursorPosition) $ toScreen screen coord
  setSGR $ toRenderSGR x
  putChar $ toRenderChar x

coordToTile :: World -> Coord -> Tile
coordToTile (World _ level) coord = lookupTile coord level

renderCoord :: Screen -> World -> Coord -> IO ()
renderCoord screen world coord = do
  render screen coord $ coordToTile world coord

renderHero :: Screen -> World -> IO ()
renderHero screen world@(World hero@(Hero curPos oldPos) _) = do
  render screen curPos hero
  if curPos == oldPos
    then return ()
    else renderCoord screen world oldPos

renderCoords :: Screen -> World -> [Coord] -> IO ()
renderCoords screen world coords = mapM_ (renderCoord screen world) coords

renderWorld :: Screen -> World -> IO ()
renderWorld screen world = do
  renderCoords screen world $ renderableCoords screen $ wLevel world
  renderHero screen world

renderableCoords :: Screen -> Level -> [Coord]
renderableCoords screen level =
  filter (onScreen screen) $ updatedCoords level

onScreen :: Screen -> Coord -> Bool
onScreen screen@(Screen _ (xMax, yMax)) coord =
  case toScreen screen coord of
    (x, y) -> 0 <= x && x < xMax && 0 <= y && y < yMax

toScreen :: Screen -> Coord -> Coord
toScreen (Screen off _) coord = coord |+| off

