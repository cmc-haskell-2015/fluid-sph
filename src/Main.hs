module Main where

import Control.Applicative
import Debug.Trace
import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

radius :: Fractional a => a
radius = 4.0

getH::Fractional a => a
getH = 0.025

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
       
-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0, 0.14))
  where
    a = 2 * pi / fromIntegral n

createFig::Point -> Int -> Particle
createFig pos a = (Particle pos (makeColor 0 0 1 1) (0,0) (0,0) 0 0 )

initpart::Int -> [Particle]
initpart n = [(createFig (x,y) 15) | x<-[100,125..(150)],y<-[100,125..(150)] ]

createWall::Path -> Point -> Float -> Point -> Wall
createWall p (x,y) f (nx,ny)= (Wall (x,y) p (makeColor 0 0 0 1) f (nx,ny))

initWalls::Int -> [Wall]
initWalls a = [(createWall [(0,0),(0,480)] (0,240) 0 (1,0)),(createWall [(640,0),(640,480)] (640,240) 180 (-1,0))
                ,(createWall [(0,0),(640,0)] (320,0) 90 (0,1)),(createWall [(0,480),(640,480)] (320,480) 90 (0,-1))]
-- | Начальный мир без фигур с заданным генератором случайных чисел.
initialWorld ::Int ->World
initialWorld a = (World (initpart 10) (initWalls 4) 0 (0,-9.81) 1)

-- | Отрисовка мира.
drawWorld :: World -> Picture
drawWorld (World fs as a _ _) = fin
  where
    pics = pictures ((F.foldMap drawParticle (reverse fs)):(F.foldMap drawWall(reverse as)):[])--(F.foldMap drawWall(reverse as))
    fpics = applyViewPortToPicture view $ pics
    fin = pictures ((applyViewPortToPicture view1 $ (drawInfo fs)):fpics:[] )
    view = ViewPort (-320,-240) a 1
    view1 = ViewPort (-320,-240) 0 1


drawInfo :: [Particle] -> Picture
drawInfo fs = pics 
  where
    pic1 = translate (350) (400) (scale (0.2) (0.2) (text ("Particles: "++show(length fs))))
    pic2 = translate (350) (450) (scale (0.15) (0.15) (text ("Press 'S' to stop/start simulation")))
    pics = pictures (pic1:pic2:[])

drawWall ::Wall -> Picture
drawWall (Wall (x,y) path c a _) = (color c (line path))

-- | Отрисовка фигуры.
drawParticle :: Particle ->  Picture
drawParticle (Particle (x, y) c _ _ _ _) = translate x y (scale 50 50 (color c ( thickCircle (radius*0.02) 0.5)))


-- | Добавить случайную фигуру в указанной точке.
addParticle :: Point -> World -> World
addParticle pos (World fs s a g sp) = (World  (f:fs) s a g sp)
  where
    f = (createFig pos 15)


wpoly6::Float -> Float
wpoly6 a = res
  where
    h = getH
    hsq = h*h
    rad = sqrt a 
    coef = 315.0 / ( 64.0 * pi * (h ^ 9) )
    res = if (a <= getH*getH ) then ( coef * ( hsq-a ) ^3 ) else 0

wpoly6spikygrad::Point->Float->Point
wpoly6spikygrad (x,y) a = result
  where
    h = getH
    coef = -45.0/(pi*(h)^6)
    rad = sqrt a
    resx = coef*(h-rad)^2*x/rad
    resy = coef*(h-rad)^2*y/rad
    result = (resx,resy)


wpoly6grad::Point->Float->Point
wpoly6grad (x,y) a = result
  where
    h = getH
    hsq = h*h
    rad = sqrt a 
    coef = -315.0 / ( 64.0 * pi * (h ^ 9) )
    resx = ( 6 * x * coef * ( hsq - a ) ^ 2) 
    resy = ( 6 * y * coef * ( hsq - a ) ^ 2)
    result = ( resx , resy )

wvisclap::Float -> Float
wvisclap a = res 
  where
    h = getH
    hsq = h*h
    rad = sqrt a 
    coef = 315.0 / ( 64.0 * pi * ( h ^ 9 ) )
    res = ( coef * 6 * ( hsq - a ) * ( 4 * a - (hsq - a )))

denscomp::Particle -> Particle -> Float
denscomp (Particle (x,y) _ _ _ _ _) (Particle (x1,y1) _ _ _ _ _)  = res
  where
    diffx = (x - x1)/1000
    diffy = (y - y1)/1000
    rq = (diffx * diffx + diffy * diffy)
    res = if rq <= getH*getH then (wpoly6 rq) else 0

compDensity ::Particle->[Particle]->Particle
compDensity (Particle (x,y) b c d dens pres) fs = fig
  where
    sum1 = (sum (map (denscomp (Particle (x,y) b c d dens pres)) fs)) * part_mass
    pr =  ( 1.5* ( sum1 - 998.0 ))
    fig = (Particle (x,y) b c d sum1 pr)

changeAngleL::World -> World
changeAngleL (World a b angle (gx,gy) s) = (World a b newangle grav s)
  where
    newangle = (angle+2)
    rot = 2/180*pi
    gx1 = gx*(cos rot)-gy*(sin rot)
    gy1 = gx*(sin rot)+gy*(cos rot)
    grav = (gx1,gy1)

changeAngleR::World -> World
changeAngleR (World a b angle (gx,gy) s) = (World a b newangle grav s)
  where
    newangle = (angle - 2)
    rot = -2/180*pi
    gx1 = gx*(cos rot)-gy*(sin rot)
    gy1 = gx*(sin rot)+gy*(cos rot)
    grav = (gx1,gy1)

defaultGrav::World -> World
defaultGrav (World a b angle (gx,gy) s) = (World a b angle1 (gx1,gy1) s)
  where
    gx1 = 0
    gy1 = -9.81 
    angle1 = 0

speedT::World -> World
speedT (World a b angle (gx,gy) s) = w
  where
    fins = if s>0 then 0 else 1
    w = (World a b angle (gx,gy) fins)

-- | Обработка событий.
handleWorld :: Event -> World -> World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) = addParticle (x+320, y+240)
handleWorld (EventKey (SpecialKey KeyLeft) Down _ (x, y)) = changeAngleL
handleWorld (EventKey (SpecialKey KeyRight) Down _ (x, y)) = changeAngleR
handleWorld (EventKey (Char 'g' ) Down _ (x, y)) = defaultGrav
handleWorld (EventKey (Char 's' ) Down _ (x, y)) = speedT
handleWorld _ = id



computeDensPres::[Particle]->[Particle]
computeDensPres f1 = (map (\x ->compDensity x f1) f1)

acccomp::Particle -> Particle -> (Point,Point)
acccomp (Particle (x,y) c (vx,vy) (ax,ay) dens pres) (Particle (x1,y1) _ (vx1,vy1) (ax1,ay1) dens1 pres1) = fig
  where
    diffx = (x - x1)/1000
    diffy = (y - y1)/1000
    rq = (diffy * diffy + diffx * diffx)
    srq = sqrt rq
    presX = (( pres + pres1) / ( 2 * dens1 ) * fst(wpoly6grad (diffx,diffy) rq))
    presY = (( pres + pres1) / ( 2 * dens1 ) * snd(wpoly6grad (diffx,diffy) rq))
    f_presx = if (( rq < getH*getH) && ( rq > 0 )) then presX else 0
    f_presy = if (( rq < getH*getH) && ( rq > 0 )) then presY else 0
    f_pres = ( f_presx , f_presy )
    f_vis1 = if (( rq < getH*getH ) && ( rq > 0 )) then (( vx1 - vx ) * (wvisclap rq) / dens1) else 0
    f_vis2 = if (( rq < getH*getH ) && ( rq > 0 )) then (( vy1 - vy ) * (wvisclap rq) / dens1) else 0
    f_vis = (f_vis1,f_vis2)
    fig =  (f_vis,f_pres)

part_mass::Fractional a => a
part_mass = 0.03

tensionCalc::Particle -> Particle -> Float
tensionCalc (Particle (x,y) c (vx,vy) (ax,ay) dens pres) (Particle (x1,y1) _ (vx1,vy1) (ax1,ay1) dens1 pres1) = ten
  where
    diffx = (x - x1)/1000
    diffy = (y - y1)/1000
    rq = (diffy * diffy + diffx * diffx)
    ten = if (( rq < getH*getH*1.3) && ( rq > 0 )) then (wvisclap rq)/dens1 else 0


compAcc::Point -> Particle->[Particle]->Particle
compAcc (gx,gy) (Particle (x,y) c (vx,vy) (ax,ay) dens pres) fs = fig
  where
    infig = (Particle (x,y) c (vx,vy) (ax,ay) dens pres)
    list = ( map ( \x-> acccomp infig x) fs)
    f_pres = map ( \x-> (snd x)) list
    f_tens = map ( \x-> tensionCalc infig x) fs
    f_tens1 = (sum f_tens)*part_mass*0.0001*5
    f_vis = map ( \x->(fst x)) list
    f_vis1 = ( sum ( map ( \x->(fst x)) f_vis)) * part_mass * (1.0)*0.0008
    f_vis2 = ( sum ( map ( \x->(snd x)) f_vis)) * part_mass * (1.0)*0.0008
    f_pres1 = ( sum ( map (fst) f_pres)) * part_mass * (-1.0)*0.007
    f_pres2 = (sum (map (snd) f_pres)) * part_mass * (-1.0)*0.007
    acc1 = (( f_pres1 + f_vis1 * 1 +gx+f_tens1),( f_pres2 + f_vis2 * 1 + gy+f_tens1))
    acc = ((fst acc1),(snd acc1))
    fig =  (Particle (x,y) c (vx,vy) acc dens pres) 

-- Вычисление сил
computeForce::[Particle] -> Point ->[Particle]
computeForce f  g = (map (\x->compAcc  g x f) f)

wall_k::Fractional a => a
wall_k = 1000

wall_d::Fractional a => a
wall_d = -10

collision::Wall ->Particle ->Point
collision (Wall (x,y) _ _ _ (nx,ny)) (Particle (x1,y1) _ (vx,vy) (ax,ay) _ _) = res
  where
    diffx = x - x1
    diffy = y - y1
    dot = nx*diffx+ny*diffy+0.01
    dampx = wall_d*(vx*nx+vy*ny)*nx
    dampy = wall_d*(vx*nx+vy*ny)*ny
    accx = wall_k*nx*dot+dampx
    accy = wall_k*ny*dot+dampy
    res = if dot>0  then (accx,accy) else (0,0)

wallColl::[Wall] -> Particle -> Point
wallColl ws (Particle (x,y) c (vx,vy) (ax,ay) dens pres) = part
  where
    p = (Particle (x,y) c (vx,vy) (ax,ay) dens pres)
    tmp = map (\x-> collision x p) ws
    tmp1 = map (\x -> (fst x)) tmp
    tmp2 = map (\x -> (snd x)) tmp
    accx = sum tmp1
    accy = sum tmp2
    part = (ax+accx,ay+accy)

-- Применение сил
applyforce::Particle  -> [Wall] -> Float -> Particle
applyforce (Particle (x,y) c (vx,vy) (ax,ay) dens pres) ws time = fig
  where
    acc = wallColl ws (Particle (x,y) c (vx,vy) (ax,ay) dens pres)
    newpos = (x + vx*time + (fst acc)*time*time/2,y + vy*time + (snd acc)*time*time/2)
    finalposx = fst newpos
    finalposy = snd newpos
    newvel =  (((finalposx-x)/time) , ((finalposy-y)/time))
    fig = (Particle newpos c newvel acc dens pres)

-- Обновление всех частиц
updateAllPart::[Wall] -> Point -> Float -> [Particle] -> [Particle]
updateAllPart ws g time part = map (\x->applyforce x ws time) ( computeForce ( computeDensPres part) g) -- map (\x->applyforce x time) 

-- Обновление мира
updateWorld :: Float -> World -> World
updateWorld time (World f s a g 1) = (World (updateAllPart s g (time*4) f) s a g 1)
updateWorld time (World f s a g sp) = (World f s a g sp)

winH::Fractional a =>a
winH = 480

winW::Fractional a => a 
winW = 640

main :: IO ()
main = do
  play display bgColor fps (initialWorld 10) drawWorld handleWorld updateWorld
  where
    windowSize   = (750, 550)
    windowOffset = (250, 250)
    display = InWindow "SPH Fluid" windowSize windowOffset
    bgColor = white
    fps = 60