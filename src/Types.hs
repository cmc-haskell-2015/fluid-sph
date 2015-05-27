-- | ???
module Types where

import Graphics.Gloss.Interface.Pure.Game

<<<<<<< Updated upstream
-- | Геометрическая фигура с координатами центра, списком вершин и цветом.
data Particle = Particle
  { pos       :: Point  -- ^ ???
  , figcolor  :: Color  -- ^ ???
  , velocity  :: Point  -- ^ ???
  , accel     :: Point  -- ^ ???
  , density   :: Float  -- ^ ???
  , pressure  :: Float  -- ^ ???
  }

-- | Стена.
data Wall = Wall
  { pos1        :: Point  -- ^ ???
  , mesh1       :: Path   -- ^ ???
  , figcolor1   :: Color  -- ^ ???
  , angle       :: Float  -- ^ ???
  , normal      :: Point  -- ^ ???
  }
=======
-- | Частица.
data Particle = Particle { 
                pos::Point,
                figcolor::Color, 
                velocity::Point, 
                acceleration::Point, 
                density::Float, 
                pressure::Float
}
-- | Стена
data Wall = Wall {
            wallPos::Point,
            linePath::Path,
            wallColor::Color,
            angle::Float,
            normal::Point
}
>>>>>>> Stashed changes

-- | Мир — множество частиц.
data World = World
  { worldParticles  :: [Particle] -- ^ ???
  , worldWalls      :: [Wall]     -- ^ ???
  , worldSomething1 :: Float      -- ^ ???
  , worldSomething2 :: Point      -- ^ ???
  , worldSomething3 :: Float      -- ^ ???
  }
