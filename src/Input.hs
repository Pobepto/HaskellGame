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
handleGame (EventKey (SpecialKey KeyLeft)  up _ _) state =
  _action state LEFT
handleGame (EventKey (SpecialKey KeyRight) up _ _) state =
  _action state RIGHT
handleGame _ state                  = state

_action :: GameState -> Direction -> GameState
_action Menu _   = Menu
_action Defeat _ = Defeat
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl) LEFT = 
  Game (Player (Position x y) (Velocity (velX - speedConst) velY) mass LEFT) pl
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl) RIGHT = 
  Game (Player (Position x y) (Velocity (velX + speedConst) velY) mass RIGHT) pl

gravity :: Float -> GameState -> GameState
gravity _  Menu                                   = Menu
gravity _  Defeat                                 = Defeat
gravity dt (Game (Player pos vel mass dir) platforms)
  | isDefeat pos vel = Defeat
  | otherwise = Game updatePlayer (updatePlatforms (Player pos vel mass dir) platforms)
  where
    updatePlayer :: Player
    updatePlayer = Player (uPos pos vel) (uVel pos vel) mass dir
    uPos :: Position -> Velocity -> Position
    uPos (Position x y) (Velocity x' y') 
      | y + y' > 0 = Position (x + x') y
      | x + x' > windowWidth = Position ((-windowWidth) + x') (y + y')
      | x + x' < -windowWidth = Position (windowWidth + x') (y + y')
      | otherwise = Position (x + x') (y + y')
    uVel :: Position -> Velocity -> Velocity
    uVel (Position x y) (Velocity x' y')
      | isCollision (Position x (y + y')) platforms = Velocity x' jumpConst
      | x' >= 0 && y' >= 0 = Velocity (x' - (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | x' <= 0 && y' >= 0 = Velocity (x' + (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | y' < 0             = Velocity x' (y' - (jumpVel * gravityConst * mass * dt))
      | otherwise          = Velocity x' y'

isCollision :: Position -> LevelPattern -> Bool
isCollision playerPos (LevelPattern platforms) = or check
  where
    check = map (\pl -> checkCollision playerPos pl) platforms

isDefeat :: Position -> Velocity -> Bool
isDefeat (Position x y) (Velocity x' y')
  | y + y' < -windowHeight = True
  | otherwise = False
