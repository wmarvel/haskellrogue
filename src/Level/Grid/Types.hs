module Level.Grid.Types where

import Coord.Types
import qualified Data.Map as M

type CellMap = M.Map Coord Cell

data Cell
  = GridEmpty    -- Nothing done - default value
  | GridFloor    -- In-Graph vertex or absence of edge
  | GridEdgeWall -- Presence of edge
  | GridEdgeHard -- Presence of an edge - cannot be cut
  | GridEdgeDoor -- Absence of edge (special)
  deriving Eq

data Grid = Grid
  { gMin :: Coord
  , gMax :: Coord
  , gCells :: CellMap
  }
  deriving Eq

instance Show Cell where
  show GridEmpty = "?"
  show GridEdgeWall = "#"
  show GridEdgeHard = "%"
  show GridEdgeDoor = "+"
  show GridFloor = "."

instance Show Grid where
  show g = foldl appendGridRow [] (cellRows g)
    where
      appendGridRow s y = s ++ (foldr showGridRow "\n" $ cellRowCoords g y)
      showGridRow c s' = (show $ cell c g) ++ s'

cell :: Coord -> Grid -> Cell
cell c g = M.findWithDefault GridEmpty c $ gCells g

-- Is a coordinate in cell space a node coordinate?
isNodeCoord :: Coord -> Bool
isNodeCoord (x, y) = (even x) && (even y)

-- Is a coordinate in cell space a link (edge) coordinate?
isLinkCoord :: Coord -> Bool
isLinkCoord (x, y) = (odd x) || (odd y)

cellRows :: Grid -> [Int]
cellRows (Grid gmin gmax _) =
  case (gridToCell gmin, gridToCell gmax) of
    ((_, y), (_, y')) -> [y..y']

cellRowCoords :: Grid -> Int -> [Coord]
cellRowCoords (Grid gmin gmax _) y =
  case (gridToCell gmin, gridToCell gmax) of
    ((x, _), (x', _)) ->
      [(x'', y) | x'' <- [x..x']]

cellCoordsUsing :: (Coord -> Bool) -> Grid -> [Coord]
cellCoordsUsing f (Grid gmin gmax _) =
  case (gridToCell gmin, gridToCell gmax) of
    ((x, y), (x', y')) ->
      [(x'', y'') | x'' <- [x .. x'], y'' <- [y .. y'], f (x'', y'')]

nodeCoords :: Grid -> [Coord]
nodeCoords (Grid (x, y) (x', y') _) =
  [(x'', y'') | x'' <- [x .. x'], y'' <- [y .. y']]

nodeCellCoords :: Grid -> [Coord]
nodeCellCoords = cellCoordsUsing isNodeCoord

linkCellCoords :: Grid -> [Coord]
linkCellCoords = cellCoordsUsing isLinkCoord

node :: Coord -> Grid -> Cell
node c = cell $ gridToCell c

gridToCell :: Coord -> Coord
gridToCell (x, y) = (x * 2, y * 2)

emptyGrid :: Coord -> Coord -> Grid
emptyGrid gmin gmax = Grid { gMin = gmin, gMax = gmax, gCells = M.empty }

emptyLinkedGrid :: Coord -> Coord -> Grid
emptyLinkedGrid gmin gmax = setAllLinks GridFloor $ emptyGrid gmin gmax

emptyUnlinkedGrid :: Coord -> Coord -> Grid
emptyUnlinkedGrid gmin gmax = setAllLinks GridEdgeWall $ emptyGrid gmin gmax

-- | Set a specific cell
setCell :: Cell -> Grid -> Coord -> Grid
setCell c g x = g { gCells = M.insert x c $ gCells g }

-- | set the edge between two adjacent cells
setLink :: Cell -> Grid -> Coord -> Coord -> Grid
setLink v g c1 c2 = setCell v g $ gridToCell c1 |+| delta
  where delta = c2 |-| c1

setNode :: Cell -> Grid -> Coord -> Grid
setNode v g c = setCell v g $ gridToCell c

setAllLinks :: Cell -> Grid -> Grid
setAllLinks c g = foldl (setCell c) g $ linkCellCoords g

-- | link two cells in grid space. They must be adjacent
link :: Grid -> Coord -> Coord -> Grid
link g c1 c2 = setLink GridFloor g c1 c2

link' :: Grid -> Coord -> Coord -> Grid
link' g c1 c2 = setLink GridEdgeDoor g c1 c2

-- | unlink two cells in grid space. They must be adjacent
unlink :: Grid -> Coord -> Coord -> Grid
unlink g c1 c2 = setLink GridEdgeWall g c1 c2

unlink' :: Grid -> Coord -> Coord -> Grid
unlink' g c1 c2 = setLink GridEdgeHard g c1 c2

-- | Visit a cell
visit :: Grid -> Coord -> Grid
visit g c = setNode GridFloor g c

-- | Carve - cut an edge and a cell
fCarve :: (Grid -> Coord -> Coord -> Grid) -> Grid -> Coord -> Coord -> Grid
fCarve f g c1 c2 = f (visit g c2) c1 c2

carve :: Grid -> Coord -> Coord -> Grid
carve = fCarve link

carve' :: Grid -> Coord -> Coord -> Grid
carve' = fCarve link'

-- Cross coordinates in unspecified space
crossCoords :: Coord -> [Coord]
crossCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Adjacent vertexes in grid space
adjacentNodes :: Coord -> [Coord]
adjacentNodes = crossCoords

-- Is inbounds in grid space
isNodeInBounds :: Grid -> Coord -> Bool
isNodeInBounds (Grid (xMin, yMin) (xMax, yMax) _) (x, y) =
  xMin <= x && yMin <= y && x <= xMax && y <= yMax

-- Is inbounds in cell space
isLinkInBounds :: Grid -> Coord -> Bool
isLinkInBounds (Grid gmin gmax _) (x, y) =
  case (gridToCell gmin, gridToCell gmax) of
    ((xMin, yMin), (xMax, yMax)) ->
      xMin <= x && yMin <= y && x <= xMax && y <= yMax

-- links in cell space of vertex in grid space
links :: Coord -> [Coord]
links = crossCoords . gridToCell


