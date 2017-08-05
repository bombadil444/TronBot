
import Graphics.Gloss

-- TODO move below vars to config.hs file
gridSizeX :: Float
gridSizeX = 800

gridSizeY :: Float
gridSizeY = 800

windowSize :: (Int, Int)
windowSize = (round gridSizeX + margin, round gridSizeY + margin)
             where margin = 50

tileSize :: Float
tileSize = 20

main :: IO ()
main = animate (InWindow "TronBot" windowSize (0, 0)) black frame

frame :: Float -> Picture
frame timeS = Pictures grid

grid :: [Picture]
grid = [ Translate
         (offset (fst pos) gridSizeX)
         (offset (snd pos) gridSizeY)
         tile
         | pos <- tilePositions ]
         where offset pos gridSize = pos - gridSize / 2 + tileSize / 2
               tilePositions = [ (x * tileSize, y * tileSize)
                                 | x <- [0..(gridSizeX / tileSize - 1)]
                                 , y <- [0..(gridSizeY / tileSize - 1)] ]

tile :: Picture
tile = Pictures [ Color white (rectangleWire tileSize tileSize) ]
