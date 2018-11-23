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

background :: Picture
background = unsafePerformIO $ loadBMP "src/assets/bg.bmp"

floorTile :: PlatformType -> Picture
-- floorTile = (unsafePerformIO $ loadBMP "src/assets/p-green.bmp")
--   <> (Color red $ rectangleWire (5 * blockSize) (1 * blockSize))
floorTile GREEN = unsafePerformIO $ loadBMP "src/assets/p-green.bmp"
floorTile BLUE  = unsafePerformIO $ loadBMP "src/assets/p-blue.bmp"
floorTile WHITE = unsafePerformIO $ loadBMP "src/assets/p-white.bmp"

playerTileL, playerTileR :: Picture
-- playerTile = (scale (0.5) (0.5) $ unsafePerformIO $ loadBMP "src/assets/doodleL.bmp")
--   <> Color red (rectangleWire (3 * blockSize) (3 * blockSize))
playerTileL = scale (0.8) (0.8) $ unsafePerformIO $ loadBMP "src/assets/doodle_s_L.bmp"
playerTileR = scale (0.8) (0.8) $ unsafePerformIO $ loadBMP "src/assets/doodle_s_R.bmp"

monsterTile :: MonsterType -> Picture
monsterTile MONSTER1 = (unsafePerformIO $ loadBMP "src/assets/monster_1.bmp")

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
simple Menu   = background <> startScreen
simple Defeat = background <> defeatScreen
simple (Game (Player (Position x y) _ _ dir) (LevelPattern pl mn)) = background
  <> drawPlatforms
  <> drawMonsters
  <> drawDirPlayer dir
  <> windowDebug
  where
    drawPlatforms :: Picture
    drawPlatforms = foldl (<>) Blank (map (\(Platform pos _ _ plType _) -> drawAt pos $ floorTile plType) pl)
    drawMonsters :: Picture
    drawMonsters = foldl (<>) Blank (map (\(Monster pos _ _ mnType) -> drawAt pos $ monsterTile mnType) mn)
    drawDirPlayer :: Direction -> Picture
    drawDirPlayer LEFT = drawAt (Position x y) playerTileL
    drawDirPlayer RIGHT = drawAt (Position x y) playerTileR
    
