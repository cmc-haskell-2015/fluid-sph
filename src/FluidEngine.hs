module FluidEngine where
import Types
import FluidConst
import Kernel
import Graphics.Gloss.Interface.Pure.Game

-- Вычисление сил
computeForce::[Particle] -> Point ->[Particle]
computeForce f  g = (map (\x->compAcc  g x f) f)


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

tensionCalc::Particle -> Particle -> Float
tensionCalc (Particle (x,y) c (vx,vy) (ax,ay) dens pres) (Particle (x1,y1) _ (vx1,vy1) (ax1,ay1) dens1 pres1) = ten
  where
    diffx = (x - x1)/1000
    diffy = (y - y1)/1000
    rq = (diffy * diffy + diffx * diffx)
    ten = if (( rq < getH*getH*1.3) && ( rq > 0 )) then (wvisclap rq)/dens1 else 0

tensionCalc2::Particle -> Particle -> Point
tensionCalc2 (Particle (x,y) c (vx,vy) (ax,ay) dens pres) (Particle (x1,y1) _ (vx1,vy1) (ax1,ay1) dens1 pres1) = res
  where
    diffx = (x - x1)/1000
    diffy = (y - y1)/1000
    rq = (diffy * diffy + diffx * diffx)
    ten = (wpoly6grad (diffx,diffy) rq)
    res1 = (fst ten) / dens1
    res2 = (snd ten) / dens1
    res = (res1,res2)

compAcc::Point -> Particle->[Particle]->Particle
compAcc (gx,gy) (Particle (x,y) c (vx,vy) (ax,ay) dens pres) fs = fig
  where
    infig = (Particle (x,y) c (vx,vy) (ax,ay) dens pres)
    list = ( map ( \x-> acccomp infig x) fs)
    f_pres = map ( \x-> (snd x)) list
    f_tensCoef = map ( \x-> tensionCalc infig x) fs
    f_tensNorm = map ( \x-> tensionCalc2 infig x) fs
    f_tensNormSum1 = map (\x->fst x) f_tensNorm 
    f_tensNormSum2 = map (\x->snd x) f_tensNorm
    tensNorm1 = (sum f_tensNormSum1) * part_mass
    tensNorm2 = (sum f_tensNormSum2) * part_mass
    f_tens1 = (sum f_tensCoef)*part_mass
    normNormal = (sqrt (tensNorm1*tensNorm1 + tensNorm2*tensNorm2))
    tensCoef = if (normNormal > 20) then (-f_tens1/normNormal) else 0
    f_tension1 = tensNorm1*tensCoef*surfCoef
    f_tension2 = tensNorm2*tensCoef*surfCoef
    f_vis = map ( \x->(fst x)) list
    f_vis1 = ( sum ( map ( \x->(fst x)) f_vis)) * part_mass * viscCoef
    f_vis2 = ( sum ( map ( \x->(snd x)) f_vis)) * part_mass * viscCoef
    f_pres1 = ( sum ( map (fst) f_pres)) * part_mass * presCoef
    f_pres2 = (sum (map (snd) f_pres)) * part_mass * presCoef
    acc1 = (( f_pres1 + f_vis1 * 1 +gx + f_tension1),( f_pres2 + f_vis2 * 1 + gy + f_tension2))
    acc = ((fst acc1),(snd acc1))
    fig =  (Particle (x,y) c (vx,vy) acc dens pres) 


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