module Fractal (mandelbrot, julia) where

import Data.Complex

import Colour (Colour, mkSafeHSV, black)

maxIter :: Double
maxIter = 128

mandelbrot :: Double -> Double -> Colour
mandelbrot x y = fractal x y (\z -> z**2 + (x:+y))

julia :: Double -> Double -> Colour
julia x y = fractal x y (\z -> z**2 + (negate 0.835 :+ negate 0.2321))

fractal :: Double -> Double -> (Complex Double -> Complex Double) -> Colour
fractal x y frac 
            | iters >= maxIter = black
            | otherwise        = mkSafeHSV iters maxIter
                where
                    iters = divergence (x:+y) 0 frac
                    
divergence :: Complex Double -> Double -> (Complex Double -> Complex Double) -> Double
divergence z n frac = go z n
            where go z' n'
                   | magnitude z' <= 2 && n' < maxIter = go (frac z') (n'+1)
                   | otherwise                         = if isNaN scaledN then maxIter else scaledN
                    where
                        scaledN = n' - (logBase 2 . logBase 2 . sqrt) (magnitude z')
