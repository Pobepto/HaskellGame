{-# LANGUAGE OverloadedStrings #-}
module Objects (
  simple
) where

import Types
import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

cnst :: Float
cnst = 10

floorTile :: Picture
--floorTile = Color green (rectangleSolid (5 * cnst)  (1 * cnst) )
floorTile = unsafePerformIO $ loadBMP "src/assets/p-green.bmp"

 
playerTile :: Picture
--playerTile = Color red (rectangleSolid (1 * cnst) (1 * cnst))
playerTile = (scale (0.5) (0.5) $ unsafePerformIO $ loadBMP "src/assets/doodleL.bmp") <> Color red (rectangleWire (3 * cnst) (3 * cnst))


blockTile :: Picture
blockTile = Color blue (rectangleSolid 1 1)

drawAt :: Position -> Picture -> Picture
drawAt (Position x y) obj = Translate (x * cnst) (y *cnst) obj

simple :: Game -> Picture
simple (Game (Player (Position x y) _ _ ) (LevelPattern pl)) =drawPlatforms <> drawAt (Position x y) playerTile 
  where
    drawPlatforms :: Picture
    drawPlatforms = foldl (<>) Blank (map (\(Platform pos _ _) -> drawAt pos floorTile) pl)
