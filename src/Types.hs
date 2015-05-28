module Types where
import Graphics.Gloss.Interface.Pure.Game

-- | Частица.
data Particle = Particle { 
                pos::Point, -- ^ Позиция.
                figcolor::Color, -- ^ Цвет.
                velocity::Point, -- ^ Скорость.
                acceleration::Point, -- ^ Ускорение.
                density::Float, -- ^ Плотность.
                pressure::Float -- ^ Давление.
}
-- | Стена.
data Wall = Wall {
            wallPos::Point, -- ^ Позиция.
            linePath::Path, -- ^ 2 точки, по которым сторим линию.
            wallColor::Color, -- ^ Цвет.
            angle::Float, -- ^ Угол поворота стены.
            normal::Point -- Нормаль стены.
}

-- | Мир — множество частиц.
data World = World [Particle] [Wall] Float Point Float
