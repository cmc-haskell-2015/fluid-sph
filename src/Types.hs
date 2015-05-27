module Types where
import Graphics.Gloss.Interface.Pure.Game

-- | Геометрическая фигура с координатами центра, списком вершин и цветом.
data Particle = Particle { 
                pos::Point,
                figcolor::Color, 
                velocity::Point, 
                accel::Point, 
                density::Float, 
                pressure::Float
}
-- Стена
data Wall = Wall {
            pos1::Point,
            mesh1::Path,
            figcolor1::Color,
            angle::Float,
            normal::Point
}

-- | Мир — множество частиц
data World = World [Particle] [Wall] Float Point Float