module Types (
  Game (Game),
  Position (Position),
  Velocity (Velocity),
  Acceleration (Acceleration),
  Player (Player),
  Platform (Platform),
  Direction (LEFT, RIGHT),
  LevelPattern (LevelPattern)
) where

data Direction = LEFT | RIGHT deriving Eq

data Game = Game
  {
    player :: Player,
    level  :: LevelPattern
  }

data Position = Position
  {
    x :: Double,
    y :: Double
  }

data Velocity = Velocity
  {
    velX :: Double,
    velY :: Double
  }

data Acceleration = Acceleration
  {
    accX :: Double,
    accY :: Double
  }
  
data LevelPattern = LevelPattern
  {
    platforms :: [Platform]
  }

data Player = Player
  {
    playerPosition     :: Position,
    playerVelocity     :: Velocity,
    playerMass         :: Double
  }

data Platform = Platform
  {
    platformPosition :: Position,
    platformWidth    :: Double,
    platformHeight   :: Double
  }
