{-# LANGUAGE OverloadedStrings #-}
module Objects (
  simple
) where

import Types
import Settings
import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

debugPosition :: Position -> Picture
debugPosition (Position x y) = printX <> printY
  where
    printX = Translate (-100) 100 (scale 0.2 0.2 (Text (show x)))
    printY = Translate 100 100 (scale 0.2 0.2 (Text (show y)))

windowDebug :: Picture
windowDebug = Color red $ rectangleWire (windowWidth * 20) (windowHeight * 20)

floorTile :: Picture
-- floorTile = (unsafePerformIO $ loadBMP "src/assets/p-green.bmp")
--   <> (Color red $ rectangleWire (5 * blockSize) (1 * blockSize))
floorTile = unsafePerformIO $ loadBMP "src/assets/p-green.bmp"

playerTile :: Picture
-- playerTile = (scale (0.5) (0.5) $ unsafePerformIO $ loadBMP "src/assets/doodleL.bmp")
--   <> Color red (rectangleWire (3 * blockSize) (3 * blockSize))
playerTile = scale (0.5) (0.5) $ unsafePerformIO $ loadBMP "src/assets/doodleL.bmp"

startScreen :: Picture
startScreen = drawAt (Position (-10) 0) 
  $ scale (0.1) (0.1) $ Text (show "Press SPACE to play game")

defeatScreen :: Picture
defeatScreen = drawAt (Position (-2.5) 0) 
  $ scale (0.1) (0.1) $ Text (show "DEFEAT")

blockTile :: Picture
blockTile = Color blue (rectangleSolid 1 1)

drawAt :: Position -> Picture -> Picture
drawAt (Position x y) obj = Translate (x * blockSize) (y * blockSize) obj

simple :: GameState -> Picture
simple Menu   = startScreen
simple Defeat = defeatScreen
simple (Game (Player (Position x y) _ _ ) (LevelPattern pl)) = drawPlatforms 
  <> drawAt (Position x y) playerTile
  where
    drawPlatforms :: Picture
    drawPlatforms = foldl (<>) Blank (map (\(Platform pos _ _) -> drawAt pos floorTile) pl)
