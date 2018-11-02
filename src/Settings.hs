module Settings (
  gravityConst,
  speedConst,
  jumpConst,
  jumpVel,
  speedVel,
  playerHeight,
  playerWidth,
  windowHeight,
  windowWidth
) where

gravityConst :: Double
gravityConst = 2

speedConst :: Double
speedConst = 0.5

jumpConst :: Double
jumpConst = 0.3

jumpVel, speedVel :: Double
jumpVel = 0.1
speedVel = 0.5

playerWidth, playerHeight :: Double
playerWidth  = 1
playerHeight = 1

windowWidth, windowHeight :: Double
windowWidth = 10
windowHeight = 10