module TronBot where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- TODO move below to config.hs file
gridSizeX = 800
gridSizeY = 800
tileSize = 20
margin = 50
startPos = (20, 20)
fps = 15

windowSize :: (Int, Int)
windowSize = (round gridSizeX + margin, round gridSizeY + margin)

gridWidth :: Int
gridWidth = round $ gridSizeX / tileSize

gridHeight :: Int
gridHeight = round $ gridSizeY / tileSize

main :: IO ()
main = let window = InWindow "TronBot" windowSize (0, 0)
       in play window black fps initialGrid drawGrid inputHandler frameHandler

data State = State { grid :: Grid
                   , currentPos :: (Int, Int)
                   , lastP :: (Int, Int)
                   , nextDirection :: Direction
                   }

data Direction = Up' | Down' | Left' | Right'

initialState :: State
initialState = State initialGrid startPos startPos Up'

initialGrid :: Grid
initialGrid = newGrid startPos startPos

drawGrid :: Grid -> Picture
drawGrid (Grid {tiles = t}) = Pictures t

inputHandler :: Event -> Grid -> Grid
inputHandler (EventKey (SpecialKey key) Down _ _) grid = move key grid
inputHandler _ grid = grid

frameHandler :: Float -> Grid -> Grid
frameHandler _ (Grid {active = (x, y),
                      lastPos = (x', y')}) = newGrid (nextPos x x' gridWidth,
                                                      nextPos y y' gridHeight)
                                                      (x, y)

nextPos :: Int -> Int -> Int -> Int
nextPos curPos oldPos limit
  | 0 <= newPos && newPos < limit   = newPos
  | otherwise                       = curPos
  where newPos = curPos + (curPos - oldPos)

move :: SpecialKey -> Grid -> Grid
move key (Grid {active = (x, y), lastPos = (x', y')})
    | key == KeyDown  && y - y' /= (-1)  && y > 0            = newGrid (x, y-1) (x, y)
    | key == KeyUp    && y - y' /= 1     && y < gridHeight-1 = newGrid (x, y+1) (x, y)
    | key == KeyLeft  && x - x' /= (-1)  && x > 0            = newGrid (x-1, y) (x, y)
    | key == KeyRight && x - x' /= 1     && x < gridWidth-1  = newGrid (x+1, y) (x, y)
    | otherwise                                              = newGrid (x, y) (x', y')

-- TODO move below to common.hs
mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

-- TODO move below to grid.hs
data Grid = Grid { tiles :: [Picture]
                 , active :: (Int, Int)
                 , lastPos :: (Int, Int)
                 } deriving(Eq, Show)

newGrid :: (Int, Int) -> (Int, Int) -> Grid
newGrid active lastPos = Grid (gridTiles active) active lastPos

posToIndex :: (Float, Float) -> (Int, Int)
posToIndex = mapTuple2 (\pos -> round $ pos / tileSize)

gridTiles :: (Int, Int) -> [Picture]
gridTiles activeIndex = [ Translate
                          (offset (fst pos) gridSizeX)
                          (offset (snd pos) gridSizeY)
                          $ gridTile $ activeIndex == posToIndex pos
                          | pos <- tilePositions ]
                          where offset pos gridSize = pos - gridSize / 2 + tileSize / 2
                                tilePositions = [ (x * tileSize, y * tileSize)
                                                  | x <- [0..(gridSizeX / tileSize - 1)]
                                                  , y <- [0..(gridSizeY / tileSize - 1)] ]

emptyTile :: Picture
emptyTile = Color white $ rectangleWire tileSize tileSize

activeTile :: Picture
activeTile = Color green $ rectangleSolid tileSize tileSize

gridTile :: Bool -> Picture
gridTile True = activeTile
gridTile False = emptyTile
