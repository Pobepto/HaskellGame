{-# LANGUAGE OverloadedStrings #-}

module Input (
  handleGame,
  gravity
) where

import CodeWorld
import Types
import Settings

handleGame :: Event -> Game -> Game
handleGame (KeyPress "Left")  game = _action game LEFT
handleGame (KeyPress "Right") game = _action game RIGHT
handleGame _ game                  = game

_action :: Game -> Direction -> Game
_action (Game (Player (Position x y) mass)) LEFT = Game (Player (Position (x - 1) y) mass)
_action (Game (Player (Position x y) mass)) RIGHT = Game (Player (Position (x + 1) y) mass)

gravity :: Double -> Game -> Game
gravity dt (Game (Player pos mass)) = Game (Player (updPos pos) mass)
  where
    updPos (Position x y)
      | isNotCollision (Position x (y - (gravityConst * mass))) = Position x (y - (gravityConst * mass))
      | otherwise       = Position x y


-- | IMPORTANT NOT FOR PRODUCTION
isNotCollision :: Position -> Bool
isNotCollision (Position x y)
  -- | y < 0 + platHeight || (x < -5 && x > 5) = True
  | x < platX + platWidth &&
      x + 1 > platX &&
      y < platY + platHeight &&
      1 + y > platY = False
  | otherwise = True
  where
    platX = -5
    platWidth = 10
    platY = 0
    platHeight = 1