module Kernel where

import FluidConst
import Graphics.Gloss.Interface.Pure.Game

-- * Ядро для плотности.
wpoly6::Float -> Float
wpoly6 a = res
  where
    h = getH
    hsq = h * h 
    coef = 315.0 / ( 64.0 * pi * (h ^ 9) )
    res = if (a <= getH * getH ) then ( coef * ( hsq - a ) ^3 ) else 0

-- * Ядро для давления №1.
wpoly6spikygrad::Point -> Float -> Point
wpoly6spikygrad (x , y) a = result
  where
    h = getH
    coef = - 45.0 / (pi * (h) ^ 6)
    rad = sqrt a
    resx = coef * (h - rad) ^ 2 * x / rad
    resy = coef * (h - rad) ^ 2 * y / rad
    result = (resx , resy)

-- * Ядро для давления №2.
wpoly6grad::Point -> Float -> Point
wpoly6grad (x , y) a = result
  where
    h = getH
    hsq = h * h
    coef = - 315.0 / ( 64.0 * pi * (h ^ 9) )
    resx = ( 6 * x * coef * ( hsq - a ) ^ 2) 
    resy = ( 6 * y * coef * ( hsq - a ) ^ 2)
    result = ( resx , resy )

-- * Ядро для вязкости и поверх натяж.
wvisclap::Float -> Float
wvisclap a = res 
  where
    h = getH
    hsq = h * h
    coef = 315.0 / ( 64.0 * pi * ( h ^ 9 ) )
    res = ( coef * 6 * ( hsq - a ) * ( 4 * a - (hsq - a )))