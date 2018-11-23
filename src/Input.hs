{-# LANGUAGE OverloadedStrings #-}

module Input (
  handleGame,
  gravity
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Types
import Settings
import Levels

handleGame :: Event -> GameState -> GameState
handleGame (EventKey (SpecialKey KeySpace) up _ _) state =
  Game
      (Player
          (Position 0 0)
          (Velocity 0 0)
          1
      )
      ( getLevel 1 )
handleGame (EventKey (SpecialKey KeyLeft)  up _ _) state =
  _action state LEFT
handleGame (EventKey (SpecialKey KeyRight) up _ _) state =
  _action state RIGHT
handleGame _ state                  = state

_action :: GameState -> Direction -> GameState
_action (Game (Player (Position x y) (Velocity velX velY) mass) pl) LEFT = 
  Game (Player (Position x y) (Velocity (velX - speedConst) velY) mass) pl
_action (Game (Player (Position x y) (Velocity velX velY) mass) pl) RIGHT = 
  Game (Player (Position x y) (Velocity (velX + speedConst) velY) mass) pl

gravity :: Float -> GameState -> GameState
gravity _  Menu                                   = Menu
gravity _  Defeat                                 = Defeat
gravity dt (Game (Player pos vel mass) platforms)
  | isDefeat pos vel = Defeat
  | otherwise = Game updatePlayer platforms
  where
    updatePlayer :: Player
    updatePlayer = Player (uPos pos vel) (uVel pos vel) mass
    uPos :: Position -> Velocity -> Position
    uPos (Position x y) (Velocity x' y') 
      | y + y' > windowHeight = Position (x + x') ((-windowHeight) + y')
      | y + y' < -windowHeight = Position (x + x') (windowHeight + y')
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
isCollision (Position x y) (LevelPattern pl) = or check
  where
    check = map (\(Platform (Position plX plY) width height) -> collision plX plY width height) pl
    collision plX plY width height
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

isDefeat :: Position -> Velocity -> Bool
isDefeat (Position x y) (Velocity x' y')
  | y + y' < -windowHeight = True
  | otherwise = False
