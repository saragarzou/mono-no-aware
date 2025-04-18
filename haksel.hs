import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Control.Concurrent (threadDelay)
import Data.Array

type Cell = Bool
type Grid = Array (Int, Int) Cell
type Coord = (Int, Int)

-- dimensions of the grid
width, height :: Int
width = 20
height = 10

-- clear screen and move cursor
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

moveCursorTopLeft :: IO ()
moveCursorTopLeft = putStr "\ESC[H"

-- display grid to console
printGrid :: Grid -> IO ()
printGrid grid = do
  moveCursorTopLeft
  mapM_ putStrLn [ [ cellChar (grid ! (x, y)) | x <- [0..width-1] ] | y <- [0..height-1] ]
  where
    cellChar True  = 'O'
    cellChar False = '.'

-- thingy to determine if coordinate is inside the grid
inBounds :: Coord -> Bool
inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

-- get neighboring coordinates
neighbors :: Coord -> [Coord]
neighbors (x, y) =
  [ (x + dx, y + dy)
  | dx <- [-1..1], dy <- [-1..1]
  , (dx, dy) /= (0, 0)
  , let newCoord = (x + dx, y + dy)
  , inBounds newCoord
  ]

-- coutn amount of neighbours that are alive
liveNeighbors :: Grid -> Coord -> Int
liveNeighbors grid coord =
  length $ filter id $ map (grid !) (neighbors coord)

-- compute next state for single cell
nextCellState :: Grid -> Coord -> Cell
nextCellState grid coord = case (alive, count) of
    (True, 2) -> True
    (_, 3)    -> True
    _         -> False
  where
    alive = grid ! coord
    count = liveNeighbors grid coord

-- generate next generation of the grid
nextGeneration :: Grid -> Grid
nextGeneration grid = array (bounds grid)
  [ (coord, nextCellState grid coord) | coord <- indices grid ]

-- initial configuration which for now just a glider
initialGrid :: Grid
initialGrid = array ((0,0), (width-1, height-1))
  [ ((x, y), (x, y) elem glider) | x <- [0..width-1], y <- [0..height-1] ]
  where
    glider = [(1,0), (2,1), (0,2), (1,2), (2,2)]

-- runs FOREVER never STOP running PLEASE
gameLoop :: Grid -> IO ()
gameLoop grid = do
  printGrid grid
  threadDelay 500000  -- small lil break of 0.5 seconds
  gameLoop (nextGeneration grid)

-- entry thingy
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  clearScreen
  gameLoop initialGrid