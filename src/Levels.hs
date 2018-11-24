module Levels (
--  getLevel,
  getInitialLevels,
  updateLevelPatter
) where

import System.Random
import Types
import Settings
import Utils

getLevel :: Int -> Int -> LevelPattern
getLevel lvl score
 | lvl == 1 && score <= 500 = level_e_1
 | lvl == 2 && score <= 500 = level_e_2
 | lvl == 3 && score <= 500 = level_e_3
 | lvl == 1 && score <= 1000 = level_m_1
 | lvl == 2 && score <= 1000 = level_m_2
 | lvl == 3 && score <= 1000 = level_m_3
 | lvl == 1 && score <= 2000 = level_h_1
 | lvl == 2 && score <= 2000 = level_h_2
 | lvl == 3 && score <= 2000 = level_h_3
 | otherwise = level_h_2

getPlatforms :: LevelPattern -> [Platform]
getPlatforms (LevelPattern platforms) = platforms


getInitialLevels :: LevelPattern
getInitialLevels = init_level

updateLevelPatter :: Player -> LevelPattern -> Int -> Int -> LevelPattern
updateLevelPatter 
  (Player (Position x y) (Velocity _ y') _ _)
  (LevelPattern platforms)
  rndLevel
  score
  | y + y' > 0 = LevelPattern 
    (filterPlatforms 
    $ ((map (\pl -> updatePlatformPosY pl) platforms)
    ++ (plusplusAccept platforms (getPlatforms (getLevel rndLevel score)))))
  | otherwise = LevelPattern (filterPlatforms (map (\pl -> updatePlatformPosX pl) platforms))
  where
    plusplusAccept :: [Platform] -> [Platform] -> [Platform]
    plusplusAccept a b
      | length a == 0 = b 
      | comparePositions (getPosition $ last a) (getPosition $ head b) = b
      | otherwise = []
    getPosition :: Platform -> Position
    getPosition (Platform pos _ _ _ _) = pos
    comparePositionsMonsters :: Position -> Position -> Bool
    comparePositionsMonsters (Position _ pl1y) (Position _ pl2y) =
      pl2y - pl1y > 25
    comparePositions :: Position -> Position -> Bool
    comparePositions (Position _ pl1y) (Position _ pl2y) =
      pl2y - pl1y > distanceOfPlatforms
    updatePlatformPosY :: Platform -> Platform
    updatePlatformPosY (Platform (Position plX plY) w h BLUE dir)
      | plX + (w / 2) > windowWidth  = Platform (Position (plX - shiftX) (plY - shiftY)) w h BLUE LEFT
      | plX - (w / 2) < -windowWidth = Platform (Position (plX + shiftX) (plY - shiftY)) w h BLUE RIGHT
      | dir == LEFT = Platform (Position (plX - shiftX) (plY - shiftY)) w h BLUE LEFT
      | otherwise = Platform (Position (plX + shiftX) (plY - shiftY)) w h BLUE RIGHT
    updatePlatformPosY (Platform (Position plX plY) w h plType dir) =
      Platform (Position plX (plY - shiftY)) w h plType dir
    updatePlatformPosX :: Platform -> Platform
    updatePlatformPosX (Platform (Position plX plY) w h BLUE dir)
      | plX + (w / 2) > windowWidth = Platform (Position (plX - shiftX) plY) w h BLUE LEFT
      | plX - (w / 2) < -windowWidth = Platform (Position (plX + shiftX) plY) w h BLUE RIGHT
      | dir == LEFT = Platform (Position (plX - shiftX) plY) w h BLUE LEFT
      | otherwise = Platform (Position (plX + shiftX) plY) w h BLUE RIGHT
    updatePlatformPosX pl = pl
    filterPlatforms :: [Platform] -> [Platform]
    filterPlatforms xs = filter (\pl -> check pl) xs
      where
        check (Platform (Position plX plY) w h plType dir) = outside && not (findWhiteCollision plType)
          where
            outside = (plY + h)> -windowHeight
            findWhiteCollision :: PlatformType -> Bool
            findWhiteCollision WHITE = 
              checkCollision
                (Position x y)
                (Platform (Position plX plY) w h plType dir)
            findWhiteCollision _ = False
    shiftY 
     | y' < 0 = 0
     | otherwise = y'
    shiftX = 0.2

init_level = LevelPattern 
    [
      (Platform (Position (-10) (-20)) 5 1 GREEN LEFT),
      (Platform (Position 0 (-20)) 5 1 GREEN LEFT),
      (Platform (Position 10 (-20)) 5 1 GREEN LEFT),
      (Platform (Position 0 0) 5 1 GREEN LEFT),
      (Platform (Position 9 (7)) 5 1 GREEN LEFT),
      (Platform (Position 9 (14)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (-10)) 5 1 GREEN LEFT),
      (Platform (Position (-13) (21)) 5 1 GREEN LEFT)
    ]

level_e_1 = LevelPattern 
    [
      (Platform (Position (0) (25)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (32)) 5 1 GREEN LEFT),
      (Platform (Position (10) (32)) 5 1 GREEN LEFT),
      (Platform (Position (0) (39)) 5 1 GREEN LEFT),
      (Platform (Position (-5) (46)) 5 1 GREEN LEFT),
      (Platform (Position (8) (53)) 5 1 GREEN LEFT)
    ]

level_e_2 = LevelPattern 
    [
      (Platform (Position (-10) (25)) 5 1 GREEN LEFT),
      (Platform (Position (7) (32)) 5 1 GREEN LEFT),
      (Platform (Position (-11) (39)) 5 1 GREEN LEFT),
      (Platform (Position (12) (46)) 5 1 GREEN LEFT),
      (Platform (Position (3) (53)) 5 1 GREEN LEFT)
    ]

level_e_3 = LevelPattern 
    [
      (Platform (Position (-10) (25)) 5 1 GREEN LEFT),
      (Platform (Position (0) (32)) 5 1 GREEN LEFT),
      (Platform (Position (10) (39)) 5 1 GREEN LEFT),
      (Platform (Position (0) (46)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (53)) 5 1 GREEN LEFT)
    ]
  

level_h_1 = LevelPattern 
    [
      (Platform (Position (-12) (25)) 5 1 WHITE LEFT),
      (Platform (Position (10) (38)) 5 1 WHITE LEFT),
      (Platform (Position (-7) (51)) 5 1 WHITE LEFT)
    ]

level_h_2 = LevelPattern 
    [
      (Platform (Position (-12) (25)) 5 1 BLUE LEFT),
      (Platform (Position (10) (38)) 5 1 BLUE RIGHT),
      (Platform (Position (-7) (51)) 5 1 BLUE LEFT)
    ]

level_h_3 = LevelPattern 
    [
      (Platform (Position (-12) (25)) 5 1 WHITE LEFT),
      (Platform (Position (10) (38)) 5 1 WHITE LEFT),
      (Platform (Position (-1) 35) 8 8 MONSTER2 LEFT),
      (Platform (Position (-7) (51)) 5 1 WHITE LEFT)
    ]


level_m_1 = LevelPattern 
    [
      (Platform (Position (-10) (25)) 5 1 GREEN LEFT),
      (Platform (Position (0) (35)) 5 1 BLUE LEFT),
      (Platform (Position (-3) 35) 6 6 MONSTER1 LEFT),
      (Platform (Position (-10) (45)) 5 1 WHITE LEFT)
    ]

level_m_2 = LevelPattern 
    [
      (Platform (Position (-12) (25)) 5 1 GREEN LEFT),
      (Platform (Position (0) (25)) 5 1 GREEN LEFT),
      (Platform (Position (0) (35)) 5 1 BLUE LEFT),
      (Platform (Position (0) (45)) 5 1 WHITE LEFT)
    ]

level_m_3 = LevelPattern 
    [
      (Platform (Position (0) (25)) 5 1 WHITE LEFT),
      (Platform (Position (-7) (35)) 5 1 GREEN LEFT),
      (Platform (Position (10) (45)) 5 1 BLUE LEFT),
      (Platform (Position (0) (55)) 5 1 GREEN LEFT)
    ]

