module Utils (
  getRandomInt,
  checkCollisionWithPlatform,
  checkCollisionWithMonster
) where

import System.Random
import Types
import Settings

getRandomInt :: StdGen -> Int -> Int -> Int
getRandomInt rnd min max = fst $ randomR (min, max) rnd

checkCollisionWithPlatform :: Position -> Platform -> Bool
checkCollisionWithPlatform (Position x y) (Platform (Position plX plY) width height _ _)
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

checkCollisionWithMonster :: Position -> Monster -> Bool
checkCollisionWithMonster (Position x y) (Monster (Position mnX mnY) width height _)
  | realPlayerX < realMonsterX + width &&
    realPlayerX + playerWidth > realMonsterX &&
    realPlayerY < realMonsterY + height &&
    playerHeight + realPlayerY > realMonsterY = True
  | otherwise = False
  where
    realPlayerX = x - (playerWidth / 2)
    realPlayerY = y - (playerHeight / 2)
    realMonsterX = mnX - (width / 2)
    realMonsterY = mnY - (height / 2)