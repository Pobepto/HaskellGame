module Objects (
  simple
) where

import CodeWorld
import Types

floorTile :: Picture
floorTile = colored black (solidRectangle 11 1)

playerTile :: Picture
playerTile = colored red (solidRectangle 1 1)

blockTile :: Picture
blockTile = colored blue (solidRectangle 1 1)

drawAt :: Position -> Picture -> Picture
drawAt (Position x y) obj = translated x y obj
  where

simple :: Game -> Picture
simple (Game (Player (Position x y) _)) = drawAt (Position 0 0) floorTile <> drawAt (Position x y) playerTile