module Utils (
  getRandomInt,
  checkCollision
) where

import System.Random
import Types
import Settings

getRandomInt :: StdGen -> Int -> Int -> Int
getRandomInt rnd min max = fst $ randomR (min, max) rnd

checkCollision:: Position -> Platform -> Bool
checkCollision (Position x y) (Platform (Position plX plY) width height t _)
  | (isMonster t) && realPlayerX < realPlatformX + width &&
    realPlayerX + playerWidth > realPlatformX &&
    realPlayerY < realPlatformY + height  &&
    playerHeight + realPlayerY > realPlatformY = True
  | not (isMonster t) && realPlayerX < realPlatformX + width &&
    realPlayerX + playerWidth > realPlatformX &&
    realPlayerY < realPlatformY + height  &&
    playerHeight / 20 + realPlayerY > realPlatformY = True
  | otherwise = False
  where
    isMonster :: PlatformType -> Bool
    isMonster t = t == MONSTER1 || t == MONSTER2 
    realPlayerX = x - (playerWidth / 2)
    realPlayerY = y - (playerHeight / 2)
    realPlatformX = plX - (width / 2)
    realPlatformY = plY - (height / 2)

