module FluidConst where
import Types
-- | Масса частицы
part_mass::Fractional a => a
part_mass = 0.03

-- | Коэффициент поверхностного натяжения
surfCoef::Fractional a => a
surfCoef = 0.0006

-- | Коэффициент вязкости
viscCoef::Fractional a => a
viscCoef = 0.0003

-- | Коэффициент силы давления
presCoef::Fractional a => a
presCoef = - 0.003

-- | Отскок от стены
wall_k::Fractional a => a
wall_k = 800

-- | Потеря энергии при отскоке
wall_d::Fractional a => a
wall_d = - 1

-- | Гравитация по-умолчанию
defaultGrav::World -> World
defaultGrav (World a b _ _ s) = (World a b angle1 (gx1,gy1) s)
  where
    gx1 = 0
    gy1 = - 9.81 
    angle1 = 0

-- | Радиус частицы
radius :: Fractional a => a
radius = 4.0

-- | Коэффициент сглаживания
getH::Fractional a => a
getH = 0.022