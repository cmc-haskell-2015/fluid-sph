module FluidConst where
import Types

part_mass::Fractional a => a
part_mass = 0.03

surfCoef::Fractional a => a
surfCoef  = 0.0006

viscCoef::Fractional a => a
viscCoef = 0.0001

presCoef::Fractional a => a
presCoef = -0.002

wall_k::Fractional a => a
wall_k = 1000

wall_d::Fractional a => a
wall_d = -10

defaultGrav::World -> World
defaultGrav (World a b angle (gx,gy) s) = (World a b angle1 (gx1,gy1) s)
  where
    gx1 = 0
    gy1 = -9.81 
    angle1 = 0

radius :: Fractional a => a
radius = 4.0

getH::Fractional a => a
getH = 0.022