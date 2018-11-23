module Input (
  handleGame,
  gravity
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Types
import Settings
import Levels
import Utils

handleGame :: Event -> GameState -> GameState
handleGame (EventKey (SpecialKey KeySpace) up _ _) state =
  Game
      (Player
          (Position 0 0)
          (Velocity 0 0)
          1
          LEFT
      )
      ( getInitialLevels )
      0
handleGame (EventKey (SpecialKey KeyLeft)  up _ _) state =
  _action state LEFT
handleGame (EventKey (SpecialKey KeyRight) up _ _) state =
  _action state RIGHT
handleGame _ state                  = state

_action :: GameState -> Direction -> GameState
_action Menu _   = Menu
_action (Defeat score) _ = (Defeat score)
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl score) LEFT = 
  Game (Player (Position x y) (Velocity (velX - speedConst) velY) mass LEFT) pl score
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl score) RIGHT = 
  Game (Player (Position x y) (Velocity (velX + speedConst) velY) mass RIGHT) pl score

gravity :: Float -> GameState -> GameState
gravity _  Menu                                   = Menu
gravity _  (Defeat score)                         = (Defeat score)
gravity dt (Game (Player pos vel mass dir) levelPattern score)
  | isDefeat pos vel levelPattern = (Defeat score)
  | otherwise = Game updatePlayer (updateLevelPatter (Player pos vel mass dir) levelPattern) score
  where
    updatePlayer :: Player
    updatePlayer = Player (uPos pos vel) (uVel pos vel) mass dir
    uPos :: Position -> Velocity -> Position
    uPos (Position x y) (Velocity x' y') 
      | x + x' > windowWidth = Position ((-windowWidth) + x') (newY)
      | x + x' < -windowWidth = Position (windowWidth + x') (newY)
      | otherwise = Position (x + x') (newY)
      where
        newY 
          | y + y' > 0 = y
          | otherwise = y + y'
    uVel :: Position -> Velocity -> Velocity
    uVel (Position x y) (Velocity x' y')
      | isCollision (Position x (y + y')) levelPattern = Velocity x' jumpConst
      | x' >= 0 && y' >= 0 = Velocity (x' - (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | x' <= 0 && y' >= 0 = Velocity (x' + (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | y' < 0             = Velocity x' (y' - (jumpVel * gravityConst * mass * dt))
      | otherwise          = Velocity x' y'

isCollision :: Position -> LevelPattern -> Bool
isCollision playerPos (LevelPattern platforms _) = or checkPl
  where
    checkPl = map (\pl -> checkCollisionWithPlatform playerPos pl) platforms

isDefeat :: Position -> Velocity -> LevelPattern -> Bool
isDefeat
  (Position x y)
  (Velocity x' y')
  (LevelPattern _ monsters)
  | y + y' < -windowHeight = True
  | or checkMn = True
  | otherwise = False
  where
    checkMn = map (\mn -> checkCollisionWithMonster (Position x y) mn) monsters
