module Level.Grid.Types where

import Coord.Types
import qualified Data.Map as M

type CellMap = M.Map Coord Cell

data Cell
  = GridEmpty -- Nothing done - default value
  | GridFloor -- vertex in-graph or presence of edge
  | GridEdgeWall -- Absence of an edge
  | GridEdgeDoor -- Presence of an edge (special)
  | GridWallHard -- vertex or edge unreachable
  deriving (Eq)

data Grid = Grid
  { gMin :: Coord
  , gMax :: Coord
  , gCells :: CellMap
  } deriving (Eq)

instance Show Cell where
  show GridEmpty = "?"
  show GridEdgeWall = "#"
  show GridWallHard = "%"
  show GridEdgeDoor = "+"
  show GridFloor = "."

instance Show Grid where
  show g = foldl appendGridRow [] (cellRows g)
    where
      appendGridRow s y = s ++ (foldr buildRow "\n" $ cellRowCoords g y)
      buildRow c s' = (show $ cell c g) ++ s'

-- Find a cell in the grid. The cell may be an edge or a vertex.
cell :: Coord -> Grid -> Cell
cell c g = M.findWithDefault GridEmpty c $ gCells g

-- Get coords in cell space of every cell in a row
rowCells :: Int -> Grid -> [Coord]
rowCells y g =
  case ((gridToCell $gMin g), (gridToCell $ gMax g)) of
    ((x, _), (x', _)) -> [(x'', 2 * y) | x'' <- [x .. x']]

colCells :: Int -> Grid -> [Coord]
colCells x g =
  case ((gridToCell $gMin g), (gridToCell $ gMax g)) of
    ((_, y), (_, y')) -> [(2 * x, y'') | y'' <- [y .. y']]

-- Is a coordinate in cell space a node (vertex) coordinate?
isNodeCoord :: Coord -> Bool
isNodeCoord (x, y) = (even x) && (even y)

-- Is a coordinate in cell space a link (edge) coordinate?
isLinkCoord :: Coord -> Bool
isLinkCoord (x, y) = (odd x) || (odd y)

cellRows :: Grid -> [Int]
cellRows (Grid gmin gmax _) =
  case (gridToCell gmin, gridToCell gmax) of
    ((_, y), (_, y')) -> [y .. y']

cellRowCoords :: Grid -> Int -> [Coord]
cellRowCoords (Grid gmin gmax _) y =
  case (gridToCell gmin, gridToCell gmax) of
    ((x, _), (x', _)) -> [(x'', y) | x'' <- [x .. x']]

allCellCoords :: Grid -> [Coord]
allCellCoords (Grid gmin gmax _) =
  [(x'', y'') | x'' <- [x .. x'], y'' <- [y .. y']]
  where
    ((x, y), (x', y')) = (gridToCell gmin, gridToCell gmax)
  
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
emptyGrid gmin gmax = Grid {gMin = gmin, gMax = gmax, gCells = M.empty}

allFloorGrid :: Coord -> Coord -> Grid
allFloorGrid gmin gmax = setAllCells GridFloor coords grid
  where
    grid = emptyGrid gmin gmax
    coords = allCellCoords grid

emptyLinkedGrid :: Coord -> Coord -> Grid
emptyLinkedGrid gmin gmax = setAllLinks GridFloor $ emptyGrid gmin gmax

emptyUnlinkedGrid :: Coord -> Coord -> Grid
emptyUnlinkedGrid gmin gmax = setAllLinks GridEdgeWall $ emptyGrid gmin gmax

bordered :: Grid -> Grid
bordered g@(Grid (xmin, ymin) (xmax, ymax) _) =
  setAllCells GridWallHard (colCells xmin g) southBordered
  where
    northBordered = setAllCells GridWallHard (rowCells ymin g) g
    eastBordered = setAllCells GridWallHard (colCells xmax g) northBordered
    southBordered = setAllCells GridWallHard (rowCells ymax g) eastBordered

-- | Set a specific cell using a cell space coordinate
setCell :: Cell -> Grid -> Coord -> Grid
setCell c g x = g {gCells = M.insert x c $ gCells g}

-- | Get the cell space coordinate of the edge between two adjacent cells
edgeCoord :: Coord -> Coord -> Coord
edgeCoord c1 c2 = gridToCell c1 |+| delta
  where
    delta = c2 |-| c1

-- | set the edge between two adjacent cells
setLink :: Cell -> Grid -> Coord -> Coord -> Grid
setLink v g c1 c2 = setCell v g $ edgeCoord c1 c2

setNode :: Cell -> Grid -> Coord -> Grid
setNode v g c = setCell v g $ gridToCell c

setAllCells :: Cell -> [Coord] -> Grid -> Grid
setAllCells v cs g = foldl (setCell v) g cs

setAllLinks :: Cell -> Grid -> Grid
setAllLinks v g = setAllCells v (linkCellCoords g) g

-- | link two cells in grid space. They must be adjacent
link :: Grid -> Coord -> Coord -> Grid
link g c1 c2 = setLink GridFloor g c1 c2

link' :: Grid -> Coord -> Coord -> Grid
link' g c1 c2 = setLink GridEdgeDoor g c1 c2

-- | unlink two cells in grid space. They must be adjacent
unlink :: Grid -> Coord -> Coord -> Grid
unlink g c1 c2 = setLink GridEdgeWall g c1 c2

unlink' :: Grid -> Coord -> Coord -> Grid
unlink' g c1 c2 = setLink GridWallHard g c1 c2

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

uncarve :: Grid -> Coord -> Coord -> Grid
uncarve g c1 c2 = unlink (setNode GridEdgeWall g c2) c1 c2

-- Cross coordinates in unspecified space
crossCoords :: Coord -> [Coord]
crossCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Adjacent vertexes in grid space
adjacentNodes :: Coord -> [Coord]
adjacentNodes = crossCoords

adjacentNodesOf :: (Cell -> Bool) -> Coord -> Grid -> [Coord]
adjacentNodesOf f x g = filter predicate (adjacentNodes x)
  where
    predicate x' = isNodeInBounds g x' && (f $ node x' g)

unlinkedNeighbors :: Coord -> Grid -> [Coord]
unlinkedNeighbors x g = filter predicate (adjacentNodes x)
  where
    predicate x' = isNodeInBounds g x' && not (isLinked x x' g)

linkedNeighbors :: Coord -> Grid -> [Coord]
linkedNeighbors x g = filter predicate (adjacentNodes x)
  where
    predicate x' = isNodeInBounds g x' && isLinked x x' g

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

-- are two coords in grid space linked?
isLinked :: Coord -> Coord -> Grid -> Bool
isLinked c1 c2 grid =
  if elem c2 $ adjacentNodes c1
  then cval == GridEdgeDoor || cval == GridFloor
  else False
  where cval = cell (edgeCoord c1 c2) grid
