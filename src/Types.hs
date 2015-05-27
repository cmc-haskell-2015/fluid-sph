module Types where
import Graphics.Gloss.Interface.Pure.Game

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

-- | Мир — множество частиц
data World = World [Particle] [Wall] Float Point Float