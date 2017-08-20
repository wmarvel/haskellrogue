module Console where

import Coord.Types
import qualified Data.Set as S
import Level
import System.Console.ANSI
import Types

data Screen = Screen
  { sOffset :: Coord
  , sSize :: (Int, Int)
  , sUpdated :: Bool
  }

data BoldInfo a = Bolded a | Normal a

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
        [SetColor Foreground Dull Magenta]
      (Dr Opened) ->
        [SetColor Foreground Dull Magenta]
      (St Down) ->
        [SetColor Foreground Dull Blue]
      (St Up) ->
        [SetColor Foreground Dull Blue]
      Wall ->
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
      Floor ->
        [SetColor Foreground Dull White]

instance ConsoleRenderable a => ConsoleRenderable (BoldInfo a) where
  toRenderChar (Normal x) = case toRenderChar x of
    '.' -> ' '
    c -> c
  toRenderChar (Bolded x) = toRenderChar x
  toRenderSGR bi = toIntensity bi : map toVivid' (toRenderSGR $ extract bi)
    where
      toVivid' = toVivid bi

extract :: BoldInfo a -> a
extract (Bolded x) = x
extract (Normal x) = x

toVivid :: BoldInfo a -> SGR -> SGR
toVivid (Bolded _) (SetColor fgbg Dull color) = SetColor fgbg Vivid color
toVivid _ x = x

toIntensity :: BoldInfo a -> SGR
toIntensity (Bolded _) = SetConsoleIntensity BoldIntensity
toIntensity _ = SetConsoleIntensity NormalIntensity

render :: (ConsoleRenderable a) => Screen -> Coord -> a -> IO ()
render screen coord x = do
  uncurry (flip setCursorPosition) $ worldToScreen screen coord
  setSGR $ toRenderSGR x
  putChar $ toRenderChar x

coordToTile :: World -> Coord -> Tile
coordToTile (World _ level) coord = lookupTile coord level

renderCoord :: Screen -> World -> Coord -> IO ()
renderCoord screen world coord = render screen coord maybeBold
  where
    tile = coordToTile world coord
    maybeBold =
      if isVisible coord world
        then Bolded tile
        else Normal tile

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
  renderCoords uScreen world $ renderableCoords uScreen world
  renderHero uScreen world
  renderStatus world
  pure uScreen
  where
    uScreen =
      if inScreenBounds screen world
        then screen {sUpdated = False}
        else updateScreen screen hero
    hero = wHero world

renderStatus :: World -> IO ()
renderStatus world = do
  setCursorPosition 20 0
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
  putStr "Depth: "
  putStr $ show $ depth * 50 + 50
  putStr "'"
  where depth = lDepth $ wLevel world

screenCoords :: Screen -> [Coord]
screenCoords screen = [(x, y) | x <- [0 .. xMax], y <- [0 .. yMax]]
  where
    (xMax, yMax) = sSize screen

renderableCoords :: Screen -> World -> [Coord]
renderableCoords screen world =
  if sUpdated screen
    then filter seen $ map (screenToWorld screen) (screenCoords screen)
    else filter seen $ filter (onScreen screen) $ updatedCoords level
  where
    seen x = S.member x $ lSeen level
    level = wLevel world

within :: Int -> Int -> Int -> Bool
within v vMin vMax = vMin <= v && v <= vMax

screenBounds :: Screen -> (Coord, Coord)
screenBounds screen = ((0, 0), sSize screen)

onScreen :: Screen -> Coord -> Bool
onScreen screen coord =
  within x xMin xMax && within y yMin yMax
  where
    (x, y) = worldToScreen screen coord
    ((xMin, yMin), (xMax, yMax)) = screenBounds screen

worldToScreen :: Screen -> Coord -> Coord
worldToScreen screen coord = coord |+| sOffset screen

screenToWorld :: Screen -> Coord -> Coord
screenToWorld screen coord = coord |-| sOffset screen

-- Hero is inbounds if all visible tiles are inbounds.
inScreenBounds :: Screen -> World -> Bool
inScreenBounds screen world =
  all (onScreen screen) $ S.toList $ lVisible $ wLevel world
      
-- translate the screen position by how much we moved in the same direction
updateScreen :: Screen -> Hero -> Screen
updateScreen _ = initialScreen
-- updateScreen screen hero = screen {sOffset = newOffset, sUpdated = True}
--    where
--     delta = (hOldPos hero) |-| (hCurPos hero)
--     newOffset = sOffset screen |+| delta

initialScreen :: Hero -> Screen
initialScreen hero = screen {sOffset = newOffset, sUpdated = True}
  where
    screen = Screen (0,0) (79,19) True
    newOffset = (halfWidth, halfHeight) |-| hCurPos hero
    (sWidth, sHeight) = sSize screen
    halfWidth = quot sWidth 2
    halfHeight = quot sHeight 2
