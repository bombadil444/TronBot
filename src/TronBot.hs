{-# LANGUAGE TemplateHaskell #-}

module TronBot where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Data.List

data Tile = Tile {
  _x       :: Int,
  _y       :: Int,
  _blocked :: Bool
} deriving (Eq, Show)

data Grid = Grid {
  _tiles    :: [Tile],
  _active   :: Tile,
  _lastPos  :: Tile,
  _nextPos  :: Tile,
  _gameover :: Bool,
  _started  :: Bool
} deriving (Eq, Show)

makeLenses ''Grid
makeLenses ''Tile

-- TODO move below to config.hs file
gridSizeX = 800
gridSizeY = 800
tileSize = 20
margin = 50
startPos = (20, 20)
fps = 15 -- TODO game speed is tied to framerate

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
initialGrid = newGrid initialTiles startPos startPos False

drawGrid :: Grid -> Picture
drawGrid grid = Pictures $ tilesToPics (grid^.tiles) (grid^.gameover)

inputHandler :: Event -> Grid -> Grid
inputHandler (EventKey (SpecialKey key) Down _ _) grid = move key grid
inputHandler _ grid = grid

frameHandler :: Float -> Grid -> Grid
frameHandler _ grid@(Grid tiles
                     act@(Tile ax ay _)
                     (Tile lx ly _)
                     next@(Tile nx ny _) gameend started
                     )
                     | gameend                                                            = grid
                     | next == act  && not (isCollision tiles (newTile defNewPos False))  = newGrid tiles defNewPos (ax, ay) started
                     |                 not (isCollision tiles next)                       = newGrid tiles (nx, ny) (ax, ay) started
                     | not started                                                        = grid
                     | otherwise                                                          = grid & gameover .~ True
                     where defNewPos = (defaultNextPos ax lx gridWidth,
                                        defaultNextPos ay ly gridHeight)

isCollision :: [Tile] -> Tile -> Bool
isCollision tiles nextPos@(Tile x y _)
    | y < 0 || y > gridHeight-1 || x < 0 || x > gridWidth-1 = True
    | gridTileBlocked tiles nextPos                         = True
    | otherwise                                             = False

defaultNextPos :: Int -> Int -> Int -> Int
defaultNextPos curPos oldPos limit
  | 0 <= newPos && newPos < limit = newPos
  | otherwise                     = curPos
  where newPos = curPos + (curPos - oldPos)

gridTileBlocked :: [Tile] -> Tile -> Bool
gridTileBlocked tiles (Tile x y _) = last [blocked | (Tile cx cy blocked) <- tiles, cx==x, cy==y]

-- this function does not update the visuals of the grid, only the positions
-- visuals are updated when framehandler is called
move :: SpecialKey -> Grid -> Grid
move key grid@(Grid _ act@(Tile ax ay _) (Tile lx ly _) next gameover _)
    | key == KeyDelete && gameover            = initialGrid
    | act /= next                             = grid
    | key == KeyDown  && abs (ay - ly) /= 1   = grid & nextPos.y -~ 1 & started .~ True
    | key == KeyUp    && abs (ay - ly) /= 1   = grid & nextPos.y +~ 1 & started .~ True
    | key == KeyLeft  && abs (ax - lx) /= 1   = grid & nextPos.x -~ 1 & started .~ True
    | key == KeyRight && abs (ax - lx) /= 1   = grid & nextPos.x +~ 1 & started .~ True
    | otherwise                               = grid

-- TODO move below to common.hs
mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

-- TODO move below to grid.hs
newGrid :: [Tile] -> (Int, Int) -> (Int, Int) -> Bool -> Grid
newGrid tiles active lastPos started = Grid (tiles++[actTile]) -- TODO tile list is growing every frame
                                       actTile lastTile actTile False started
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

tilesToPics :: [Tile] -> Bool -> [Picture]
tilesToPics tiles gameover = [ tileToPic tile gameover | tile <- tiles ]

tileToPic :: Tile -> Bool -> Picture
tileToPic tile@(Tile x y blocked) gameover = Translate
    (offset (fromIntegral x) gridSizeX)
    (offset (fromIntegral y) gridSizeY)
    $ gridTile blocked gameover
    where offset pos gridSize = pos * tileSize - gridSize / 2 + tileSize / 2

emptyTilePic :: Picture
emptyTilePic = Color white $ rectangleWire tileSize tileSize

blockedTilePic :: Picture
blockedTilePic = Color green $ rectangleSolid tileSize tileSize

gameoverTilePic :: Picture
gameoverTilePic = Color red $ rectangleSolid tileSize tileSize

gridTile :: Bool -> Bool -> Picture
gridTile blocked gameover
    | gameover && blocked = gameoverTilePic
    | blocked             = blockedTilePic
    | otherwise           = emptyTilePic
