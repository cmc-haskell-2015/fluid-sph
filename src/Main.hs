module Main where

import Control.Applicative
import Debug.Trace
import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game

-- | Для генерации случайных значений используем целые числа.
data Gen = Gen Int deriving (Show)

-- | Случайное значение типа a.
data Rand a = Rand (Gen -> (a, Gen))

-- | Получить случайное значение, подав на вход зерно генератора случайных чисел.
getRand :: Rand a -> Gen -> (a, Gen)
getRand (Rand f) = f

instance Functor Rand where
  fmap f (Rand g) = Rand (\n -> let (x, n') = g n in (f x, n'))

instance Applicative Rand where
  pure x = Rand (\n -> (x, n))
  Rand f <*> Rand g = Rand (\n -> let (x, n') = f n
                                      (y, n'') = g n'
                                  in (x y, n''))

-- | Сгенерировать случайное число от 0 до maxRand.
--
-- Для генерации используется линейный конгруэнтный метод:
--   X(n+1) = (a * Xn + c) mod m
rand :: Rand Int
rand = Rand (\g -> let g' = next g in (res g', g'))
  where
    res  (Gen n) = n `mod` maxRand
    next (Gen n) = Gen ((a * n + c) `mod` m)
    (a, c, m) = (1366, 150889, 714025)

-- | Максимальное случайное число, сгенерированное при помощи rand.
maxRand :: Num a => a
maxRand = 2^16

radius :: Fractional a => a
radius = 0.02

-- | Геометрическая фигура с координатами центра, списком вершин и цветом.
data Figure = Figure Point Path Color Point Point Float Float

-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0, 0.05))
  where
    a = 2 * pi / fromIntegral n

createFig::Point -> Figure
createFig pos = (Figure pos (equilateral 15) (makeColor 1 0 0 1) (0,-1.8) (0,0) 0 0 )
-- | Мир — множество фигур и генератор случайных ч
data World = World Gen [Figure]

interectPart ::  Figure -> Float -> [Figure] -> [Figure]
interectPart _ _ [] = []
interectPart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) delta ((Figure (x0,y0) path0 c0 (vx0,vy0) (ax0,ay0) ab0 ac0) : xs) 
  | (((lengthPart (x,y) (x0,y0) - delta) < (2*radius)) && ((lengthPart (x,y) (x0,y0)) == 0)) = ((Figure (x0,y0) path0 c0 (vx0,vy0) (ax0,ay0) ab0 ac0) : (interectPart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) delta xs))
  | otherwise = (interectPart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) delta xs)

lengthPart :: Point -> Point -> Float
lengthPart (x1,y1) (x2,y2) = sqrt ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

initpart::Int -> [Figure]
initpart n = [(createFig (x,y)) | x<-[-200,-150..(-100)],y<-[-200,-150..(-100)] ]

-- | Начальный мир без фигур с заданным генератором случайных чисел.
initialWorld :: Gen -> World
initialWorld g = World g (initpart 10)

-- | Отрисовка мира.
drawWorld :: World -> Picture
drawWorld (World _ fs) = F.foldMap drawFigure (reverse fs)

-- | Отрисовка фигуры.
drawFigure :: Figure -> Picture
drawFigure (Figure (x, y) path c _ _ _ _) = translate x y (scale 50 50 (color c (polygon path)))

-- | Добавить случайную фигуру в указанной точке.
addFigure :: Point -> World -> World
addFigure pos (World g fs) = World g' (f:fs)
  where
    (f, g') = ((createFig pos),g)

wpoly6::Float -> Float
wpoly6 a = coef*(hsq-a)^3
  where
    h=0.045
    hsq=h*h
    coef=315.0/(64.0*3.1415*(h^9))

wpoly6grad::Point->Float->Point
wpoly6grad (x,y) a = (x*coef*(hsq-a)^2,y*coef*(hsq-a)^2)
  where
    h=0.045
    hsq=h*h
    coef= -945.0/(32.0*3.1415*(h^9))

wvisclap::Float -> Float
wvisclap a = coef*(h-rad)
  where
    h=0.045
    coef=45.0/(3.1415*(h^6))
    rad=a*a

denscomp::Figure->Figure->Float
denscomp (Figure (x,y) _ _ _ _ _ _) (Figure (x1,y1) _ _ _ _ _ _)  = c
  where
    diffx=x-x1
    diffy=y-y1
    rq=diffx*diffx+diffy*diffy
    c= if rq<=0.045*0.045 then ((wpoly6 rq)*0.02*(0.045*0.45 -rq)^3)else 0

compDensity ::Figure->[Figure]->Float
compDensity f fs = (foldl (+) 0 (map (denscomp f) fs))*0.02

compVisc::Figure ->Figure->Point
compVisc (Figure (x,y) _ _ (vx,vy) _ dens _) (Figure (x1,y1) _ _ (vx1,vy1) _ dens1 _)=res
  where
    diffx=x-x1
    diffy=y-y1
    rq=diffx*diffx+diffy*diffy
    lap=wvisclap rq
    finx=(vx-vx1)*lap/dens1
    finy=(vy-vy1)*lap/dens1
    res=(finx,finy)

compPres::Figure ->Figure->Point
compPres (Figure (x,y) _ _ _ _ dens pres) (Figure (x1,y1) _ _ _ _ dens1 pres1)=res
  where
    diffx=x-x1
    diffy=y-y1
    rq=diffx*diffx+diffy*diffy
    grad=wpoly6grad (diffx,diffy) rq
    finx = (pres+pres1)/(2*dens1)*fst(grad)
    finy = (pres+pres1)/(2*dens1)*snd(grad)
    --finx = (dens/1000.0)^7-1
    res=(if rq <=0.045*0.045 && rq >0.0 then finx else 0,if rq <=0.045*0.045 && rq >0.0 then finy else 0)
    
updatePart2::Figure -> [Figure] -> Float-> Figure
updatePart2 (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) fs time= (Figure pos path c vel acc ab ac)
  where
    f = (Figure (x,y) path c (vx,vy) (ax,ay) ab ac)
    --f_pres=(foldl (\x f1 ->((fst(x)+fst(f1))*(-0.02),(snd(x)+snd(f1))*(-0.02))) (0,0) (map (compPres f) fs))
    --f_vis=(foldl (\x f1->( (fst(x)+fst(f1))*(-0.02*3.5),(snd(x)+snd(f1)))) (0,0) (map (compVisc f) fs))
    f_pres = (0,0)
    f_vis = (0,0)
    finx = (fst(f_pres)+fst(f_vis)+0.0)/(ab+0.00001)
    finy = (snd(f_pres)+snd(f_vis)+(-1.8))/(ab+0.00001)
    acc =(finx,finy)
    delta = 1.0/100.0
    posx= (x+vx+ax*time)
    posy= y + vy+ay*time
    velx = vx+time*finx
    vely = vy+time*finy
    pos= ((max (posx) (-200.0)),(max (posy) (-200.0)))
    vel = (velx,vely)


updatePart::Figure -> Float -> [Figure]-> Figure
updatePart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) time  fs = (updatePart2 (Figure (x,y) path c (vx,vy) (ax,ay) dens pres) (interectPart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) 0.05 fs) time)
  where
    dens = (compDensity  (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) (interectPart (Figure (x,y) path c (vx,vy) (ax,ay) ab ac) 0.05 fs))
    pres = 3.0*(dens - 0.0)
    --acc=(ax-0,ay-1.8)
    --vel=(vx+time*fst(acc),vy+time*snd(acc))
    --pos=(max (x+time*fst(vel)) (-200.0), (max (y+time*snd(vel)) (-200.0)))

-- | Обработка событий.
handleWorld :: Event -> World -> World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) = addFigure (x, y)
handleWorld _ = id

-- | Обновление мира (ничего не происходит).
updateWorld :: Float -> World -> World
updateWorld time (World g f) = (World g (map (\x->updatePart x (time/10000) f ) f))

main :: IO ()
main = do
  play display bgColor fps (initialWorld seed) drawWorld handleWorld updateWorld
  where
    windowSize   = (640, 480)
    windowOffset = (200, 200)
    display = InWindow "Random figures" windowSize windowOffset
    bgColor = white
    fps = 60
    seed = Gen 31337
