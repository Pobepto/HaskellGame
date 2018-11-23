module Utils (
  getRandomInt,
  checkCollision
) where

import System.Random
import Types
import Settings

getRandomInt :: StdGen -> Int -> Int -> Int
getRandomInt rnd min max = fst $ randomR (min, max) rnd

checkCollision :: Position -> Platform -> Bool
checkCollision (Position x y) (Platform (Position plX plY) width height _ _)
  | realPlayerX < realPlatformX + width &&
    realPlayerX + playerWidth > realPlatformX &&
    realPlayerY < realPlatformY + height &&
    playerHeight + realPlayerY > realPlatformY = True
  | otherwise = False
  where
    realPlayerX = x - (playerWidth / 2)
    realPlayerY = y - (playerHeight / 2)
    realPlatformX = plX - (width / 2)
    realPlatformY = plY - (height / 2)