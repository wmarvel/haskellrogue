module Console where

import Coord.Types
import qualified Data.Set as S
import Level
import System.Console.ANSI
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
  uncurry (flip setCursorPosition) $ worldToScreen screen coord
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
renderCoords screen world coords = do
  if sUpdated screen
    then clearScreen
    else pure ()
  mapM_ (renderCoord screen world) coords

renderWorld :: Screen -> World -> IO Screen
renderWorld screen world = do
  renderCoords uScreen world $ renderableCoords uScreen $ wLevel world
  renderHero uScreen world
  pure $ uScreen
  where
    uScreen =
      if inScreenBounds screen hero
        then screen {sUpdated = False}
        else updateScreen screen hero
    hero = wHero world

screenCoords :: Screen -> [Coord]
screenCoords screen = [(x, y) | x <- [0 .. xMax], y <- [0 .. yMax]]
  where
    (xMax, yMax) = sSize screen

renderableCoords :: Screen -> Level -> [Coord]
renderableCoords screen level =
  if sUpdated screen
    then filter seen $ map (screenToWorld screen) (screenCoords screen)
    else filter seen $ filter (onScreen screen) $ updatedCoords level
  where
    seen x = S.member x $ lSeen level

within :: Int -> Int -> Int -> Bool
within v vMin vMax = vMin <= v && v <= vMax

screenBounds :: Screen -> (Coord, Coord)
screenBounds screen = ((0, 0), sSize screen)

onScreen :: Screen -> Coord -> Bool
onScreen screen coord =
  case worldToScreen screen coord of
    (x, y) -> within x xMin xMax && within y yMin yMax
  where
    ((xMin, yMin), (xMax, yMax)) = screenBounds screen

worldToScreen :: Screen -> Coord -> Coord
worldToScreen screen coord = coord |+| sOffset screen

screenToWorld :: Screen -> Coord -> Coord
screenToWorld screen coord = coord |-| sOffset screen

inScreenBounds :: Screen -> Hero -> Bool
inScreenBounds screen hero =
  within xHero (xMin + 1) (xMax - 1) && within yHero (yMin + 1) (yMax - 1)
  where
    (xHero, yHero) = worldToScreen screen $ hCurPos hero
    ((xMin, yMin), (xMax, yMax)) = screenBounds screen

-- For now we will just set up an offset that centers the screen
-- on our hero. We can do fancier translations later
updateScreen :: Screen -> Hero -> Screen
updateScreen screen hero = screen {sOffset = newOffset, sUpdated = True}
  where
    newOffset = (halfWidth, halfHeight) |-| hCurPos hero
    (sWidth, sHeight) = sSize screen
    halfWidth = quot sWidth 2
    halfHeight = quot sHeight 2
