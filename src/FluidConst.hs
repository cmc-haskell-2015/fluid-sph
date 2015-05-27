-- | ???
module FluidConst where

import Types
<<<<<<< Updated upstream

-- | ???
part_mass::Fractional a => a
part_mass = 0.03

-- | ???
=======
-- | Масса частицы
part_mass::Fractional a => a
part_mass = 0.03

-- | Коэффициент поверхностного натяжения
>>>>>>> Stashed changes
surfCoef::Fractional a => a
surfCoef = 0.0006

<<<<<<< Updated upstream
-- | ???
=======
-- | Коэффициент вязкости
>>>>>>> Stashed changes
viscCoef::Fractional a => a
viscCoef = 0.0003

<<<<<<< Updated upstream
-- | ???
=======
-- | Коэффициент силы давления
>>>>>>> Stashed changes
presCoef::Fractional a => a
presCoef = - 0.003

<<<<<<< Updated upstream
-- | ???
=======
-- | Отскок от стены
>>>>>>> Stashed changes
wall_k::Fractional a => a
wall_k = 800

<<<<<<< Updated upstream
-- | ???
=======
-- | Потеря энергии при отскоке
>>>>>>> Stashed changes
wall_d::Fractional a => a
wall_d = - 1

<<<<<<< Updated upstream
-- | ???
=======
-- | Гравитация по-умолчанию
>>>>>>> Stashed changes
defaultGrav::World -> World
defaultGrav (World a b _ _ s) = (World a b angle1 (gx1,gy1) s)
  where
    gx1 = 0
    gy1 = - 9.81 
    angle1 = 0

<<<<<<< Updated upstream
-- | ???
radius :: Fractional a => a
radius = 4.0

-- | ???
=======
-- | Радиус частицы
radius :: Fractional a => a
radius = 4.0

-- | Коэффициент сглаживания
>>>>>>> Stashed changes
getH::Fractional a => a
getH = 0.022
