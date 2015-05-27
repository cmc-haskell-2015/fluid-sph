module Main where

import Types
import FluidEngine
import FluidConst

import Control.Applicative

import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort


       
-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0 , 0.14))
  where
    a = 2 * pi / fromIntegral n

-- | Создает частицу в заданной позиции
createFig::Point -> Particle
createFig posit  = (Particle posit (makeColor 0 0 1 1) (0,0) (0,0) 0 0 )

-- | Создает список частиц
initpart::Int -> [Particle]
initpart _ = [(createFig (x , y)) | x <- [100,125..(350)] , y <- [100,125..(350)] ]

-- | Создает стену с параметрами в указанной точке
createWall::Path -> Point -> Float -> Point -> Wall
createWall p (x , y) f (nx , ny)= (Wall (x , y) p (makeColor 0 0 0 1) f (nx , ny))

-- | Создает список стен
initWalls::Int -> [Wall]
initWalls a = [(createWall [(0 , 0) , (0 , 480)] (0 , 240) 0 (1 , 0)) , (createWall [(640 , 0) , (640 , 480)] (640 , 240) 180 ( - 1 , 0))
                , (createWall [(0 , 0) , (640 , 0)] (320 , 0) 90 (0 , 1)) , (createWall [(0 , 480) , (640 , 480)] (320 , 480) 90 (0 , - 1))]

-- | Начальный мир с частицами и стенами
initialWorld ::Int -> World
initialWorld a = (World (initpart 10) (initWalls 4) 0 (0 , - 9.81) 1)

-- | Отрисовка мира.
drawWorld :: World -> Picture
drawWorld (World fs as a _ _) = fin
  where
    pics = pictures ((F.foldMap drawParticle (reverse fs)) : (F.foldMap drawWall(reverse as)) : [])
    fpics = applyViewPortToPicture view $ pics
    fin = pictures ((applyViewPortToPicture view1 $ (drawInfo fs)) : fpics : [] )
    view = ViewPort ( - 320 , - 240) a 1
    view1 = ViewPort ( - 320 , - 240) 0 1

-- | Вывод информации
drawInfo :: [Particle] -> Picture
drawInfo fs = pics 
  where
    pic1 = translate (350) (400) (scale (0.2) (0.2) (text ("Particles: "++show(length fs))))
    pic2 = translate (350) (450) (scale (0.15) (0.15) (text ("Press 'S' to stop/start simulation")))
    pics = pictures (pic1 : pic2 : [])

-- | Отрисовка стен
drawWall ::Wall -> Picture
drawWall (Wall _ path c _ _) = (color c (line path))

-- | Отрисовка частицы.
drawParticle :: Particle ->  Picture
drawParticle (Particle (x , y) c _ _ _ _) = translate x y (scale 50 50 (color c ( thickCircle (radius * 0.02) 0.25)))


-- | Добавить частицу в указанной точке.
addParticle :: Point -> World -> World
addParticle posit (World fs s a g sp) = (World  (f : fs) s a g sp)
  where
    f = (createFig posit)


-- | Поворот "коробки" влево
changeAngleL::World -> World
changeAngleL (World a b angleW (gx,gy) s) = (World a b newangle grav s)
  where
    newangle = (angleW + 2)
    rot = 2 / 180 * pi
    gx1 = gx * (cos rot) - gy * (sin rot)
    gy1 = gx * (sin rot) + gy * (cos rot)
    grav = (gx1 , gy1)

-- | Поворот "коробки" вправо
changeAngleR::World -> World
changeAngleR (World a b angleW (gx , gy) s) = (World a b newangle grav s)
  where
    newangle = (angleW - 2)
    rot = - 2 / 180 * pi
    gx1 = gx * (cos rot) - gy * (sin rot)
    gy1 = gx * (sin rot) + gy * (cos rot)
    grav = (gx1 , gy1)

-- | Пауза/Продолжить
speedT::World -> World
speedT (World a b angleW (gx , gy) s) = w
  where
    fins = if s > 0 then 0 else 1
    w = (World a b angleW (gx , gy) fins)

-- | Обработка событий.
handleWorld :: Event -> World -> World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x , y)) = addParticle (x + 320 , y + 240)
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) = changeAngleL
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) = changeAngleR
handleWorld (EventKey (Char 'g' ) Down _ _) = defaultGrav
handleWorld (EventKey (Char 's' ) Down _ _) = speedT
handleWorld _ = id

-- Обновление всех частиц
updateAllPart::[Wall] -> Point -> Float -> [Particle] -> [Particle]
updateAllPart ws g time part = map (\x -> applyforce x ws time) ( computeForce ( computeDensPres part) g)

-- Обновление мира
updateWorld :: Float -> World -> World
updateWorld time (World f s a g 1) = (World (updateAllPart s g (time * 4) f) s a g 1)
updateWorld _ (World f s a g sp) = (World f s a g sp)

-- | Высота окнв
winH::Fractional a =>a
winH = 480

-- | Ширина окна
winW::Fractional a => a 
winW = 640

main :: IO ()
main = do
  play display bgColor fps (initialWorld 10) drawWorld handleWorld updateWorld
  where
    windowSize   = (750 , 550)
    windowOffset = (250 , 250)
    display = InWindow "SPH Fluid" windowSize windowOffset
    bgColor = white
    fps = 60