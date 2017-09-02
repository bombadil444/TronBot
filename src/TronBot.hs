{-# LANGUAGE TemplateHaskell #-}

module TronBot where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens

data Tile = Tile {
  _x      :: Int,
  _y      :: Int,
  _blocked :: Bool
} deriving (Eq, Show)

data Grid = Grid {
  _tiles    :: [Tile],
  _active  :: Tile,
  _lastPos  :: Tile,
  _nextPos  :: Tile
} deriving (Eq, Show)

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

initialGrid :: Grid
initialGrid = newGrid initialTiles startPos startPos

drawGrid :: Grid -> Picture
drawGrid grid = Pictures (tilesToPics $ grid^.tiles)

inputHandler :: Event -> Grid -> Grid
inputHandler (EventKey (SpecialKey key) Down _ _) grid = move key grid
inputHandler _ grid = grid

frameHandler :: Float -> Grid -> Grid
frameHandler _ grid@(Grid tiles
                     act@(Tile ax ay _)
                     (Tile lx ly _)
                     next@(Tile nx ny _)
                     )
                     | next == act   = newGrid tiles defNewPos (ax, ay)
                     | otherwise     = newGrid tiles (nx, ny) (ax, ay)
                     where defNewPos = (defaultNextPos ax lx gridWidth,
                                        defaultNextPos ay ly gridHeight)

defaultNextPos :: Int -> Int -> Int -> Int
defaultNextPos curPos oldPos limit
  | 0 <= newPos && newPos < limit   = newPos
  | otherwise                       = curPos
  where newPos = curPos + (curPos - oldPos)

-- this function does not update the visuals of the grid, only the positions
-- visuals are updated when framehandler is called
move :: SpecialKey -> Grid -> Grid
move key grid@(Grid _ act@(Tile ax ay _) (Tile lx ly _) next)
    | act /= next                                                 = grid
    | key == KeyDown  && (abs ay - ly) /= 1  && ay > 0            = grid & nextPos.y -~ 1
    | key == KeyUp    && (abs ay - ly) /= 1  && ay < gridHeight-1 = grid & nextPos.y +~ 1
    | key == KeyLeft  && (abs ax - lx) /= 1  && ax > 0            = grid & nextPos.x -~ 1
    | key == KeyRight && (abs ax - lx) /= 1  && ax < gridWidth-1  = grid & nextPos.x +~ 1
    | otherwise                                                   = grid

-- TODO move below to common.hs
mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

-- TODO move below to grid.hs
newGrid :: [Tile] -> (Int, Int) -> (Int, Int) -> Grid
newGrid tiles active lastPos = Grid (tiles++[actTile])
                                    actTile lastTile actTile
                                    where actTile  = newTile active True
                                          lastTile = newTile lastPos True

posToIndex :: (Float, Float) -> (Int, Int)
posToIndex = mapTuple2 (\pos -> round $ pos / tileSize)

newTile :: (Int, Int) -> Bool -> Tile
newTile (x, y) active = Tile x y active

initialTiles :: [Tile]
initialTiles = [ newTile (posToIndex (x * tileSize, y * tileSize)) False
                 | x <- [0..(gridSizeX / tileSize) - 1 ]
                 , y <- [0..(gridSizeY / tileSize) - 1 ] ]

tilesToPics :: [Tile] -> [Picture]
tilesToPics tiles = [ tileToPic tile | tile <- tiles ]

tileToPic :: Tile -> Picture
tileToPic tile@(Tile x y blocked) = Translate
    (offset (fromIntegral x) gridSizeX)
    (offset (fromIntegral y) gridSizeY)
    $ gridTile blocked
    where offset pos gridSize = pos * tileSize - gridSize / 2 + tileSize / 2


emptyTilePic :: Picture
emptyTilePic = Color white $ rectangleWire tileSize tileSize

blockedTilePic :: Picture
blockedTilePic = Color green $ rectangleSolid tileSize tileSize

gridTile :: Bool -> Picture
gridTile True = blockedTilePic
gridTile False = emptyTilePic
