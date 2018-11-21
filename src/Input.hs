{-# LANGUAGE OverloadedStrings #-}

module Input (
  handleGame,
  gravity
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Types
import Settings

handleGame :: Event -> Game -> Game
handleGame (EventKey (SpecialKey KeyLeft) up _ _ )  game = _action game LEFT
handleGame (EventKey (SpecialKey KeyRight) up _ _) game = _action game RIGHT
handleGame _ game                  = game

_action :: Game -> Direction -> Game
_action (Game (Player (Position x y) (Velocity velX velY) mass) pl) LEFT = 
  Game (Player (Position x y) (Velocity (velX - speedConst) velY) mass) pl
_action (Game (Player (Position x y) (Velocity velX velY) mass) pl) RIGHT = 
  Game (Player (Position x y) (Velocity (velX + speedConst) velY) mass) pl

gravity :: Float -> Game -> Game
gravity dt (Game (Player pos vel mass) platforms) = Game updatePlayer platforms
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
      | x < realX + (width + 1) &&
        x + playerWidth > realX &&
        y < plY + height &&
        playerHeight + y > plY = True
      | otherwise = False
      where
        realX = plX + (-width / 2)
