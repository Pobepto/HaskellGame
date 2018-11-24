module Levels (
  getLevel,
  getInitialLevels,
  updateLevelPatter
) where

import System.Random
import Types
import Settings
import Utils

getLevel :: Int -> LevelPattern
getLevel level_id
  | level_id <= 5 = level_1
  | otherwise = init_level

getPlatforms :: LevelPattern -> [Platform]
getPlatforms (LevelPattern platforms _) = platforms

getMonsters :: LevelPattern -> [Monster]
getMonsters (LevelPattern _ monsters) = monsters

combineLevels :: LevelPattern -> LevelPattern -> LevelPattern
combineLevels (LevelPattern pl1 mn1) (LevelPattern pl2 mn2) = 
  LevelPattern pl mn
  where
    pl = pl1 ++ pl2
    mn = mn1 ++ mn2

getInitialLevels :: LevelPattern
getInitialLevels = init_level

-- random level
getInitialLevels1 :: LevelPattern
getInitialLevels1 = level_e_1

updateLevelPatter :: Player -> LevelPattern -> LevelPattern
updateLevelPatter 
  (Player (Position x y) (Velocity _ y') _ _)
  (LevelPattern platforms monsters)
  | y + y' > 0 = LevelPattern 
    (filterPlatforms 
    $ ((map (\pl -> updatePlatformPosY pl) platforms)
    ++ (plusplusAccept platforms (getPlatforms getInitialLevels1))))
    (filterMonsters
    $ map (\mn -> updateMonsterPosY mn) monsters)
  | otherwise = LevelPattern (filterPlatforms (map (\pl -> updatePlatformPosX pl) platforms)) monsters
  where
    plusplusAccept :: [Platform] -> [Platform] -> [Platform]
    plusplusAccept a b
      | comparePositions (getPosition $ last a) (getPosition $ head b) = b
      | otherwise = []
    getPosition :: Platform -> Position
    getPosition (Platform pos _ _ _ _) = pos
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
    updateMonsterPosY :: Monster -> Monster
    updateMonsterPosY (Monster (Position mnX mnY) w h mnType) =
      Monster (Position mnX (mnY - shiftY)) w h mnType
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
              checkCollisionWithPlatform 
                (Position x y)
                (Platform (Position plX plY) w h plType dir)
            findWhiteCollision _ = False
    filterMonsters :: [Monster] -> [Monster]
    filterMonsters xs = filter (\(Monster (Position _ y) _ h _) -> (y + h)  > -windowHeight) xs
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
    []

level_e_1 = LevelPattern 
    [
      (Platform (Position (0) (25)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (32)) 5 1 GREEN LEFT),
      (Platform (Position (10) (32)) 5 1 GREEN LEFT),
      (Platform (Position (0) (39)) 5 1 GREEN LEFT),
      (Platform (Position (-5) (46)) 5 1 GREEN LEFT),
      (Platform (Position (8) (53)) 5 1 GREEN LEFT)
    ]
    []
  
level_1 = LevelPattern 
    [
      (Platform (Position (-10) (-20)) 5 1 GREEN LEFT),
      (Platform (Position 0 (-20)) 5 1 GREEN LEFT),
      (Platform (Position 10 (-20)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (-6)) 5 1 GREEN LEFT)
    ]
    [
    ]
  
level_3 = LevelPattern 
    [
      (Platform (Position (-10) (-20)) 5 1 GREEN LEFT),
      (Platform (Position 0 (-20)) 5 1 GREEN LEFT),
      (Platform (Position 10 (-20)) 5 1 GREEN LEFT),
      (Platform (Position (-10) (-6)) 5 1 GREEN LEFT)
    ]
    [
    ]
  