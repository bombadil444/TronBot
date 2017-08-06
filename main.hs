
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- TODO move below to config.hs file
gridSizeX = 800
gridSizeY = 800
tileSize = 20
startPos = (20, 20)

windowSize :: (Int, Int)
windowSize = (round gridSizeX + margin, round gridSizeY + margin)
             where margin = 50

main :: IO ()
main = let window = InWindow "TronBot" windowSize (0, 0)
       in play window black 1 initialGrid drawGrid inputHandler (\_ world -> world)

initialGrid :: Grid
initialGrid = newGrid startPos

drawGrid :: Grid -> Picture
drawGrid (Grid {tiles = t}) = Pictures t

inputHandler :: Event -> Grid -> Grid
inputHandler (EventKey (SpecialKey key) Down _ _) grid = move key grid
inputHandler _ grid = grid

move :: SpecialKey -> Grid -> Grid
move key (Grid {active = (x, y)})
    | key == KeyDown    = newGrid (x, y-1)
    | key == KeyUp      = newGrid (x, y+1)
    | key == KeyLeft    = newGrid (x-1, y)
    | key == KeyRight   = newGrid (x+1, y)
    | otherwise         = newGrid (x, y)

-- TODO move below to common.hs
mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

-- TODO move below to grid.hs
data Grid = Grid { tiles :: [Picture]
                 , active :: (Int, Int)
                 }

newGrid :: (Int, Int) -> Grid
newGrid active = Grid (gridTiles active) active

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
