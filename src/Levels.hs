module Levels (
  getLevel,
  getInitialLevels,
  updatePlatforms
) where

import System.Random
import Types
import Settings
import Utils

level_2 = LevelPattern [
      -- (Platform (Position (-4) (20)) 5 1),
      -- (Platform (Position 4 (15)) 5 1),

      -- (Platform (Position (-4) (10)) 5 1),
      -- (Platform (Position 4 (5)) 5 1),
      -- (Platform (Position (-4) (0)) 5 1)
      (Platform (Position 4 (-5)) 5 1 GREEN LEFT),
      (Platform (Position (-5) (0)) 5 1 WHITE LEFT)
      -- (Platform (Position (-4) (-10)) 5 1),
      -- (Platform (Position 4 (-15)) 5 1)
      --(Platform (Position 0 0) 5 1)
    ]

level_1 = LevelPattern [
  (Platform (Position 0 5) 5 1 WHITE LEFT),
  (Platform (Position 0 15) 5 1 WHITE LEFT),
  (Platform (Position 0 10) 5 1 WHITE LEFT),
    (Platform (Position 0 (-10)) 5 1 WHITE LEFT),
    (Platform (Position 0 0) 5 1 WHITE LEFT),
    (Platform (Position 0 (-5)) 5 1 WHITE LEFT)
    ]

level_3 = LevelPattern [
      -- (Platform (Position (-4) 30) 5 1 BLUE LEFT),
      (Platform (Position 0 25) 5 1 WHITE LEFT)
    ]

getLevel :: Int -> LevelPattern
getLevel level_id
  | level_id <= 5 = level_1
  | otherwise = level_2

getPlatforms :: LevelPattern -> [Platform]
getPlatforms (LevelPattern platforms) = platforms

getInitialLevels :: LevelPattern
getInitialLevels = LevelPattern combine
  where
    combine = (getPlatforms level_1) ++ (getPlatforms level_2)

getInitialLevels1 :: LevelPattern
getInitialLevels1 = LevelPattern combine
  where
    combine = (getPlatforms level_3)

updatePlatforms :: Player -> LevelPattern -> LevelPattern
updatePlatforms (Player (Position x y) (Velocity _ y') _ _) (LevelPattern platforms)
  | y + y' > 0 = LevelPattern 
    $ filterPlatforms 
    $ ((map (\pl -> updatePosY pl) platforms)
    ++ (plusplusAccept platforms (getPlatforms getInitialLevels1)))
  | otherwise = LevelPattern 
    $ filterPlatforms (map (\pl -> updatePosX pl) platforms)
  where
    plusplusAccept :: [Platform] -> [Platform] -> [Platform]
    plusplusAccept a b
      | comparePositions (getPosition $ last a) (getPosition $ head b) = b
      | otherwise = []
    getPosition :: Platform -> Position
    getPosition (Platform pos _ _ _ _) = pos
    comparePositions :: Position -> Position -> Bool
    comparePositions (Position _ pl1y) (Position _ pl2y) = abs (pl1y - pl2y) > 10
    updatePosY :: Platform -> Platform
    updatePosY (Platform (Position plX plY) w h BLUE dir)
      | plX + (w / 2) > windowWidth  = Platform (Position (plX - shiftX) (plY - shiftY)) w h BLUE LEFT
      | plX - (w / 2) < -windowWidth = Platform (Position (plX + shiftX) (plY - shiftY)) w h BLUE RIGHT
      | dir == LEFT = Platform (Position (plX - shiftX) (plY - shiftY)) w h BLUE LEFT
      | otherwise = Platform (Position (plX + shiftX) (plY - shiftY)) w h BLUE RIGHT

    updatePosY (Platform (Position plX plY) w h plType dir)
      = Platform (Position plX (plY - shiftY)) w h plType dir
    updatePosX :: Platform -> Platform
    updatePosX (Platform (Position plX plY) w h BLUE dir)
      | plX + (w / 2) > windowWidth = Platform (Position (plX - shiftX) plY) w h BLUE LEFT
      | plX - (w / 2) < -windowWidth = Platform (Position (plX + shiftX) plY) w h BLUE RIGHT
      | dir == LEFT = Platform (Position (plX - shiftX) plY) w h BLUE LEFT
      | otherwise = Platform (Position (plX + shiftX) plY) w h BLUE RIGHT
    updatePosX pl = pl
    filterPlatforms :: [Platform] -> [Platform]
    filterPlatforms xs = filter (\pl -> check pl) xs
      where
        check (Platform (Position plX plY) w h plType dir) = outside && not (findWhiteCollision plType)
          where
            outside = plY > -windowHeight
            findWhiteCollision :: PlatformType -> Bool
            findWhiteCollision WHITE = checkCollision (Position x y) (Platform (Position plX plY) w h plType dir)
            findWhiteCollision _ = False
    shiftY 
      | y' < 0 = 0
      | otherwise = y'
    shiftX = 0.2
