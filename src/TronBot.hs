{-# LANGUAGE TemplateHaskell #-}

module TronBot where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens

data Tile = Tile {
  _x  :: Int,
  _y :: Int
} deriving (Eq, Show)

data Grid = Grid {
  _tiles    :: [Picture],
  _active   :: Tile,
  _lastPos  :: Tile
} deriving (Show)

-- TODO replace grid world reference with state
data State = State {
  grid           :: Grid,
  currentPos     :: (Int, Int),
  lastP          :: (Int, Int),
  nextDirection  :: Direction
}

data Direction = Up' | Down' | Left' | Right'

makeLenses ''Grid
makeLenses ''Tile

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

initialState :: State
initialState = State initialGrid startPos startPos Up'

initialGrid :: Grid
initialGrid = newGrid startPos startPos

drawGrid :: Grid -> Picture
drawGrid grid = Pictures $ grid^.tiles

inputHandler :: Event -> Grid -> Grid
inputHandler (EventKey (SpecialKey key) Down _ _) grid = move key grid
inputHandler _ grid = grid

frameHandler :: Float -> Grid -> Grid
frameHandler _ grid = newGrid newPos (act^.x, act^.y) -- TODO use lens set instead
                      where act    = grid^.active
                            last   = grid^.lastPos
                            newPos = ( nextPos (act^.x) (last^.x) gridWidth,
                                       nextPos (act^.y) (last^.y) gridHeight)

nextPos :: Int -> Int -> Int -> Int
nextPos curPos oldPos limit
  | 0 <= newPos && newPos < limit   = newPos
  | otherwise                       = curPos
  where newPos = curPos + (curPos - oldPos)

move :: SpecialKey -> Grid -> Grid
move key grid
    | key == KeyDown  && (abs ay - ly) /= 1  && ay > 0            = newGrid (ax, ay-1) (ax, ay) -- TODO use lens set instead
    | key == KeyUp    && (abs ay - ly) /= 1  && ay < gridHeight-1 = newGrid (ax, ay+1) (ax, ay)
    | key == KeyLeft  && (abs ax - lx) /= 1  && ax > 0            = newGrid (ax-1, ay) (ax, ay)
    | key == KeyRight && (abs ax - lx) /= 1  && ax < gridWidth-1  = newGrid (ax+1, ay) (ax, ay)
    | otherwise                                                   = grid
    where ax = grid^.active.x
          ay = grid^.active.y
          lx = grid^.lastPos.x
          ly = grid^.lastPos.y

-- TODO move below to common.hs
mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

-- TODO move below to grid.hs

newGrid :: (Int, Int) -> (Int, Int) -> Grid
newGrid active lastPos = Grid (gridTiles (newTile active))
                              (newTile active)
                              (newTile lastPos)

posToTile :: (Float, Float) -> Tile
posToTile t = newTile $ posToIndex t

posToIndex :: (Float, Float) -> (Int, Int)
posToIndex = mapTuple2 (\pos -> round $ pos / tileSize)

newTile :: (Int, Int) -> Tile
newTile (x, y) = Tile x y

gridTiles :: Tile -> [Picture]
gridTiles activeTile = [ Translate
                          (offset (fst pos) gridSizeX)
                          (offset (snd pos) gridSizeY)
                          $ gridTile $ activeTile == posToTile pos
                          | pos <- tilePositions ]
                          where offset pos gridSize = pos - gridSize / 2 + tileSize / 2
                                tilePositions = [ (x * tileSize, y * tileSize)
                                                  | x <- [0..(gridSizeX / tileSize - 1)]
                                                  , y <- [0..(gridSizeY / tileSize - 1)] ]

emptyTilePic :: Picture
emptyTilePic = Color white $ rectangleWire tileSize tileSize

activeTilePic :: Picture
activeTilePic = Color green $ rectangleSolid tileSize tileSize

gridTile :: Bool -> Picture
gridTile True = activeTilePic
gridTile False = emptyTilePic
