module Settings (
  blockSize,
  gravityConst,
  speedConst,
  jumpConst,
  jumpVel,
  speedVel,
  playerHeight,
  playerWidth,
  windowHeight,
  windowWidth,
  distanceOfPlatforms
) where

gravityConst :: Float
gravityConst = 2

blockSize :: Float
blockSize = 10

speedConst :: Float
speedConst = 0.1

jumpConst :: Float
jumpConst = 0.3

jumpVel, speedVel :: Float
jumpVel = 0.1
speedVel = 0.1

playerWidth, playerHeight :: Float
playerWidth  = 3
playerHeight = 3

windowWidth, windowHeight :: Float
windowWidth = 16
windowHeight = 24

distanceOfPlatforms :: Float
distanceOfPlatforms = 10