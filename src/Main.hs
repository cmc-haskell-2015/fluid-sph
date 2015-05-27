module Main where

import Types
import FluidEngine
import FluidConst

import Control.Applicative
import Debug.Trace
import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0, 0.14))
  where
    a = 2 * pi / fromIntegral n

-- | ???
createFig::Point -> Int -> Particle
createFig pos a = (Particle pos (makeColor 0 0 1 1) (0,0) (0,0) 0 0 )

-- | ???
initpart::Int -> [Particle]
initpart n = [(createFig (x,y) 15) | x<-[100,125..(350)],y<-[100,125..(350)] ]

-- | ???
createWall::Path -> Point -> Float -> Point -> Wall
createWall p (x,y) f (nx,ny) = (Wall (x,y) p (makeColor 0 0 0 1) f (nx,ny))

-- | ???
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

-- | ???
drawInfo :: [Particle] -> Picture
drawInfo fs = pics 
  where
    pic1 = translate (350) (400) (scale (0.2) (0.2) (text ("Particles: "++show(length fs))))
    pic2 = translate (350) (450) (scale (0.15) (0.15) (text ("Press 'S' to stop/start simulation")))
    pics = pictures (pic1:pic2:[])

-- | ???
drawWall ::Wall -> Picture
drawWall (Wall (x,y) path c a _) = (color c (line path))

-- | Отрисовка фигуры.
drawParticle :: Particle ->  Picture
drawParticle (Particle (x, y) c _ _ _ _) = translate x y (scale 50 50 (color c ( thickCircle (radius*0.02) 0.25)))

-- | Добавить случайную фигуру в указанной точке.
addParticle :: Point -> World -> World
addParticle pos (World fs s a g sp) = (World  (f:fs) s a g sp)
  where
    f = (createFig pos 15)

-- | ???
changeAngleL::World -> World
changeAngleL (World a b angle (gx,gy) s) = (World a b newangle grav s)
  where
    newangle = (angle+2)
    rot = 2/180*pi
    gx1 = gx*(cos rot)-gy*(sin rot)
    gy1 = gx*(sin rot)+gy*(cos rot)
    grav = (gx1,gy1)

-- | ???
changeAngleR::World -> World
changeAngleR (World a b angle (gx,gy) s) = (World a b newangle grav s)
  where
    newangle = (angle - 2)
    rot = -2/180*pi
    gx1 = gx*(cos rot)-gy*(sin rot)
    gy1 = gx*(sin rot)+gy*(cos rot)
    grav = (gx1,gy1)

-- | ???
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

-- | Обновление всех частиц.
updateAllPart::[Wall] -> Point -> Float -> [Particle] -> [Particle]
updateAllPart ws g time part = map (\x->applyforce x ws time) ( computeForce ( computeDensPres part) g)

-- | Обновление мира.
updateWorld :: Float -> World -> World
updateWorld time (World f s a g 1) = (World (updateAllPart s g (time*4) f) s a g 1)
updateWorld time (World f s a g sp) = (World f s a g sp)

-- | ???
winH::Fractional a =>a
winH = 480

-- | ???
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
