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

gravityConst :: Float
gravityConst = 2

speedConst :: Float
speedConst = 0.1

jumpConst :: Float
jumpConst = 0.3

jumpVel, speedVel :: Float
jumpVel = 0.1
speedVel = 0.5

playerWidth, playerHeight :: Float
playerWidth  = 3
playerHeight = 3

windowWidth, windowHeight :: Float
windowWidth = 10
windowHeight = 10
