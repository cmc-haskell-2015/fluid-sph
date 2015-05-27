module FluidEngine where
import Types
import FluidConst
import Kernel
import Graphics.Gloss.Interface.Pure.Game

-- | Вычисление сил.
computeForce::[Particle] -> Point ->[Particle]
computeForce f g = (map (\x -> compAcc  g x f) f)

-- | Вычисление плотности для 2 частиц.
denscomp::Particle -> Particle -> Float
denscomp (Particle (x , y) _ _ _ _ _) (Particle (x1 , y1) _ _ _ _ _)  = res
  where
    diffx = (x - x1) / 1000
    diffy = (y - y1) / 1000
    rq = (diffx * diffx + diffy * diffy)
    res = if rq <= getH * getH then (wpoly6 rq) else 0

-- | Вычисление плотности для всех частиц.
compDensity ::Particle -> [Particle] -> Particle
compDensity (Particle (x , y) b c d dens pres) fs = fig
  where
    sum1 = (sum (map (denscomp (Particle (x , y) b c d dens pres)) fs)) * part_mass
    pr =  ( 1.5 * ( sum1 - 998.0 ))
    fig = (Particle (x , y) b c d sum1 pr)

-- | Вычисление плотности и давления для всех частиц.
computeDensPres::[Particle] -> [Particle]
computeDensPres f1 = (map (\x -> compDensity x f1) f1)


-- | Вычисление силы давления и вязкости для 2 частиц.
acccomp::Particle -> Particle -> (Point,Point)
acccomp (Particle (x , y) _ (vx , vy) _ _ pres) (Particle (x1 , y1) _ (vx1 , vy1) _ dens1 pres1) = fig
  where
    diffx = (x - x1) / 1000
    diffy = (y - y1) / 1000
    rq = (diffy * diffy + diffx * diffx)
    presX = (( pres + pres1) / ( 2 * dens1 ) * fst(wpoly6grad (diffx,diffy) rq))
    presY = (( pres + pres1) / ( 2 * dens1 ) * snd(wpoly6grad (diffx,diffy) rq))
    f_presx = if (( rq < getH * getH) && ( rq > 0 )) then presX else 0
    f_presy = if (( rq < getH * getH) && ( rq > 0 )) then presY else 0
    f_pres = ( f_presx , f_presy )
    f_vis1 = if (( rq < getH * getH ) && ( rq > 0 )) then (( vx1 - vx ) * (wvisclap rq) / dens1) else 0
    f_vis2 = if (( rq < getH * getH ) && ( rq > 0 )) then (( vy1 - vy ) * (wvisclap rq) / dens1) else 0
    f_vis = (f_vis1 , f_vis2)
    fig =  (f_vis , f_pres)


-- | Вычисление силы поверхностного натяжения для 2 частиц.
tensionCalc::Particle -> Particle -> Float
tensionCalc (Particle (x , y) _ _ _ _ _) (Particle (x1 , y1) _ _ _ dens1 _) = ten
  where
    diffx = (x - x1) / 1000
    diffy = (y - y1) / 1000
    rq = (diffy * diffy + diffx * diffx)
    ten = if (( rq < getH * getH * 1.3) && ( rq > 0 )) then (wvisclap rq) / dens1 else 0

-- | Вспмогательная функция.
tensionCalc2::Particle -> Particle -> Point
tensionCalc2 (Particle (x , y) _ _ _ _ _) (Particle (x1 , y1) _ _ _ dens1 _) = res
  where
    diffx = (x - x1) / 1000
    diffy = (y - y1) / 1000
    rq = (diffy * diffy + diffx * diffx)
    ten = (wpoly6grad (diffx,diffy) rq)
    res1 = (fst ten) / dens1
    res2 = (snd ten) / dens1
    res = (res1 , res2)

-- | Рассчет ускорения для частицы с учетом сил вязкости, давления и поверх. натяжения.
compAcc::Point -> Particle -> [Particle] -> Particle
compAcc (gx , gy) (Particle (x , y) c (vx , vy) (ax , ay) dens pres) fs = fig
  where
    infig = (Particle (x , y) c (vx , vy) (ax , ay) dens pres)
    list = ( map ( \a -> acccomp infig a) fs)
    f_pres = map ( \a -> (snd a)) list
    f_tensCoef = map ( \a -> tensionCalc infig a) fs
    f_tensNorm = map ( \b -> tensionCalc2 infig b) fs
    f_tensNormSum1 = map (\a -> fst a) f_tensNorm 
    f_tensNormSum2 = map (\b -> snd b) f_tensNorm
    tensNorm1 = (sum f_tensNormSum1) * part_mass
    tensNorm2 = (sum f_tensNormSum2) * part_mass
    f_tens1 = (sum f_tensCoef)*part_mass
    normNormal = (sqrt (tensNorm1*tensNorm1 + tensNorm2*tensNorm2))
    tensCoef = if (normNormal > 20) then ( - f_tens1 / normNormal) else 0
    f_tension1 = tensNorm1 * tensCoef * surfCoef
    f_tension2 = tensNorm2 * tensCoef * surfCoef
    f_vis = map ( \a -> (fst a)) list
    f_vis1 = ( sum ( map ( \a -> (fst a)) f_vis)) * part_mass * viscCoef
    f_vis2 = ( sum ( map ( \a -> (snd a)) f_vis)) * part_mass * viscCoef
    f_pres1 = ( sum ( map (fst) f_pres)) * part_mass * presCoef
    f_pres2 = (sum (map (snd) f_pres)) * part_mass * presCoef
    acc1 = (( f_pres1 + f_vis1 * 1 +gx + f_tension1),( f_pres2 + f_vis2 * 1 + gy + f_tension2))
    acc = ((fst acc1) , (snd acc1))
    fig =  (Particle (x , y) c (vx , vy) acc dens pres) 

-- | Вспомогательная функция для столкновеня со стенами.
collision::Wall -> Particle -> Point
collision (Wall (x , y) _ _ _ (nx , ny)) (Particle (x1 , y1) _ (vx , vy) _ _ _) = res
  where
    diffx = x - x1
    diffy = y - y1
    dot = nx * diffx + ny * diffy + 0.01
    dampx = wall_d * (vx * nx + vy * ny) * nx
    dampy = wall_d * (vx * nx + vy * ny) * ny
    accx = wall_k * nx * dot + dampx
    accy = wall_k * ny * dot + dampy
    res = if dot > 0  then (accx , accy) else (0 , 0)

-- | Столновения со стенами.
wallColl::[Wall] -> Particle -> Point
wallColl ws (Particle (x , y) c (vx , vy) (ax , ay) dens pres) = part
  where
    p = (Particle (x , y) c (vx , vy) (ax , ay) dens pres)
    tmp = map (\a -> collision a p) ws
    tmp1 = map (\a -> (fst a)) tmp
    tmp2 = map (\a -> (snd a)) tmp
    accx = sum tmp1
    accy = sum tmp2
    part = (ax + accx , ay + accy)

-- Применение сил.
applyforce::Particle  -> [Wall] -> Float -> Particle
applyforce (Particle (x , y) c (vx , vy) (ax , ay) dens pres) ws time = fig
  where
    acc = wallColl ws (Particle (x , y) c (vx , vy) (ax , ay) dens pres)
    newpos = (x + vx * time + (fst acc) * time * time / 2 , y + vy * time + (snd acc) * time * time / 2)
    finalposx = fst newpos
    finalposy = snd newpos
    newvel =  (((finalposx - x) / time) , ((finalposy - y) / time))
    fig = (Particle newpos c newvel acc dens pres)