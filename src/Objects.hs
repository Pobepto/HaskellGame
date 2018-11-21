module Objects (
  simple
) where

import CodeWorld
import Types

floorTile :: Picture
floorTile = colored black (solidRectangle 5 1)

playerTile :: Picture
playerTile = colored red (solidRectangle 1 1)

blockTile :: Picture
blockTile = colored blue (solidRectangle 1 1)

drawAt :: Position -> Picture -> Picture
drawAt (Position x y) obj = translated x y obj

simple :: Game -> Picture
simple (Game (Player (Position x y) _ _ ) (LevelPattern pl)) = drawAt (Position x y) playerTile <> drawPlatforms
  where
    drawPlatforms :: Picture
    drawPlatforms = foldl (<>) blank (map (\(Platform pos _ _) -> drawAt pos floorTile) pl)
