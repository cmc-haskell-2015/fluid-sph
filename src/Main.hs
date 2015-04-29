module Main where

import Control.Applicative
import Debug.Trace
import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game


radius :: Fractional a => a
radius = 4.0

-- | Геометрическая фигура с координатами центра, списком вершин и цветом.
data Figure = Figure { 
                pos::Point, 
                mesh::Path,
                figcolor::Color, 
                velocity::Point, 
                accel::Point, 
                density::Float, 
                pressure::Float
}

data Wall = Wall {
            pos1::Point,
            mesh1::Path,
            figcolor1::Color,
            angle::Float
}

-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0, 0.14))
  where
    a = 2 * pi / fromIntegral n

createFig::Point -> Int -> Figure
createFig pos a = (Figure pos (equilateral a) (makeColor 0 0 1 1) (0,0) (0,-9.8) 0 0 )
-- | Мир — множество фигур и генератор случайных ч
data World = World [Figure] [Wall]

initpart::Int -> [Figure]
initpart n = [(createFig (x,y) 15) | x<-[-200,-185..(-170)],y<-[-100,-85..(-70)] ]

createWall::Point -> Float -> Wall
createWall (x,y) = (Wall (x,y) (equilateral 4) (makeColor 0 0 0 1) )

initWalls::Int -> [Wall]
initWalls a = [(createWall (-140,-100) 0),(createWall (-260,-100) 180),(createWall (-200,-40) 90),(createWall (-200,-160) 90)]
-- | Начальный мир без фигур с заданным генератором случайных чисел.
initialWorld ::Int ->World
initialWorld a = (World (initpart 10) (initWalls 4))

-- | Отрисовка мира.
drawWorld :: World -> Picture
drawWorld (World fs as) = pictures ((F.foldMap drawFigure (reverse fs)):(F.foldMap drawWall(reverse as)):[])


drawWall :: Wall -> Picture
drawWall (Wall (x,y) path c a)= translate x y (rotate a (scale 10 670 (color c (polygon path))))

-- | Отрисовка фигуры.
drawFigure :: Figure ->  Picture
drawFigure (Figure (x, y) path c _ _ _ _)= translate x y (scale 50 50 (color c (polygon path)))


-- | Добавить случайную фигуру в указанной точке.
addFigure :: Point -> World -> World
addFigure pos (World fs s) = (World  (f:fs) s)
  where
    f = (createFig pos 15)


wpoly6::Float -> Float
wpoly6 a = res
  where
    h = 0.75
    hsq = 2 * radius * radius
    rad = sqrt a 
    coef = 315.0 / ( 64.0 * pi * (h ^ 9) )
    res = if (rad <= 5 * radius ) then ( coef * ( hsq-a ) ^3 ) else 0

wpoly6grad::Point->Float->Point
wpoly6grad (x,y) a = result
  where
    h = 1.54
    hsq = radius * radius
    rad = sqrt a 
    coef = -315.0 / ( 64.0 * pi * (h ^ 9) )
    resx = if (rad <= 5 * radius ) then  ( 6 * x * coef * ( hsq - a ) ^ 2) else 0 
    resy = if (rad <= 5 * radius ) then  ( 6 * y * coef * ( hsq - a ) ^ 2) else 0 
    result = ( resx , resy )

wvisclap::Float -> Float
wvisclap a = res 
  where
    h = 1.1
    hsq = radius * radius
    rad = sqrt a 
    coef = 315.0 / ( 64.0 * pi * ( h ^ 9 ) )
    res = if (rad <= 5 * radius ) then  ( coef * 6 * ( hsq - a ) * ( 4 * a - (hsq - a ))) else 0

denscomp::Figure -> Figure -> Float
denscomp (Figure (x,y) _ _ _ _ _ _) (Figure (x1,y1) _ _ _ _ _ _)  = res
  where
    diffx = x - x1
    diffy = y - y1
    rq = diffx * diffx + diffy * diffy
    res = if rq <= 10 * radius * radius then (wpoly6 rq) else 0

compDensity ::Figure->[Figure]->Figure
compDensity (Figure (x,y) a b c d dens pres) fs = fig
  where
    sum1 = (sum (map (denscomp (Figure (x,y) a b c d dens pres)) fs)) * part_mass
    pr = ( 3.0 * ( sum1 - 998.0 ))
    fig = (Figure (x,y) a b c d sum1 pr)

-- | Обработка событий.
handleWorld :: Event -> World -> World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) = addFigure (x, y)
handleWorld _ = id

computeDensPres::[Figure]->[Figure]
computeDensPres f1 = (map (\x ->compDensity x f1) f1)

acccomp::Figure -> Figure -> (Point,Point)
acccomp (Figure (x,y) p c (vx,vy) (ax,ay) dens pres) (Figure (x1,y1) _ _ (vx1,vy1) (ax1,ay1) dens1 pres1) = fig
  where
    diffx = x - x1 
    diffy = y - y1
    rq = diffy * diffy + diffx * diffx
    presX = (( pres + pres1) / ( 2 * dens1 ) * fst(wpoly6grad (diffx,diffy) rq))
    presY = (( pres + pres1) / ( 2 * dens1 ) * snd(wpoly6grad (diffx,diffy) rq))
    f_presx = if ((rq < 10 * radius * radius) && ( rq > 0 )) then presX else 0
    f_presy = if ((rq < 10 * radius * radius) && ( rq > 0 )) then presY else 0
    f_pres = ( f_presx , f_presy )
    f_vis1 = if (( rq < 20 * radius * radius ) && ( rq > 0 )) then (( vx1 - vx ) * (wvisclap rq) / dens1) else 0
    f_vis2 = if (( rq < 20 * radius * radius ) && ( rq > 0 )) then (( vy1 - vy ) * (wvisclap rq) / dens1) else 0
    f_vis = (f_vis1,f_vis2)
    fig =  (f_vis,f_pres)
    --продолжить!!!!!!!!!!!!!!!!

part_mass::Fractional a=>a
part_mass = 0.003

compAcc::Figure->[Figure]->Figure
compAcc (Figure (x,y) p c (vx,vy) (ax,ay) dens pres) fs = fig
  where
    infig = (Figure (x,y) p c (vx,vy) (ax,ay) dens pres)
    list = ( map ( \x-> acccomp infig x) fs)
    f_pres = map ( \x-> (snd x)) list
    f_vis = map ( \x->(fst x)) list
    f_vis1 = ( sum ( map ( \x->(fst x)) f_vis)) * part_mass * (-1.0)
    f_vis2 = ( sum ( map ( \x->(snd x)) f_vis)) * part_mass * (-1.0)
    f_pres1 = ( sum ( map (fst) f_pres)) * part_mass * (-1.0)*0.9
    f_pres2 = (sum (map (snd) f_pres)) * part_mass * (-1.0)*0.9
    f_grav = -9.81
    acc1 = (( f_pres1 + f_vis1 * 3.1 ),( f_pres2 + f_vis2 * 3.1 + f_grav))
    acc = ((fst acc1),(snd acc1))
    fig = (Figure (x,y) p c (vx,vy) acc dens pres) 
 
computeForce::[Figure]->[Figure]
computeForce f = (map (\x->compAcc x f) f)


applyforce::Figure -> Float -> Figure
applyforce (Figure (x,y) p c (vx,vy) (ax,ay) dens pres) time = fig
  where
    newpos = (x + vx*time + ax*time*time/2,y + vy*time + ay*time*time/2)
    finalposx1 = if ((fst newpos)<=(-250)) then (-250) else (fst newpos)
    finalposy1 = if ((snd newpos)<=(-150)) then (-150) else (snd newpos)
    finalposx = if (finalposx1 >= (-150)) then (-150) else finalposx1
    finalposy = if (finalposy1 >= (-50)) then (-50) else finalposy1
    
    finalvelx = if ((finalposx<=(-250)) && (finalposx >= -150))then ((-1)*(finalposx-x)/time) else ((finalposx-x)/time)
    finalvely = if ((finalposy<=(-150)) && (finalposy >= -50)) then ((-1)*(finalposy1-y)/time) else ((finalposy1-y)/time)
    
    finalaccx = if ((finalposx<=(-250)) && (finalposx >= -150)) then ((-1)*ax) else ax
    finalaccy = if ((finalposy<=(-150)) && (finalposy >= -50)) then ((-1)*ay) else ay


    newvel = (finalvelx,finalvely)
    fig = (Figure (finalposx,finalposy) p c newvel (finalaccx,finalaccy) dens pres)

updateAllPart::Float -> [Figure] -> [Figure]
updateAllPart time part = map (\x->applyforce x time) ( computeForce ( computeDensPres part))

-- | Обновление мира
updateWorld :: Float -> World -> World
updateWorld time (World f s) = (World (updateAllPart (time*3) f) s)


main :: IO ()
main = do
  play display bgColor fps (initialWorld 10) drawWorld handleWorld updateWorld
  where
    windowSize   = (640, 480)
    windowOffset = (250, 250)
    display = InWindow "SPH Fluid" windowSize windowOffset
    bgColor = white
    fps = 60