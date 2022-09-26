module Fractal (Fractal, fractals) where

import Data.Complex

import Drawing (Drawer(draw))
import Colour (Colour, mkSafeHSV, black)

data Fractal = Mandelbrot | Julia | BurningShip
                deriving (Read, Show, Enum, Bounded)

fractals :: [Fractal]
fractals = [(minBound :: Fractal) ..]

instance Drawer Fractal where
        draw Mandelbrot x y  = fractal x y (\z -> z**2 + (x:+y))
        draw Julia x y       = fractal x y (\z -> z**2 + (negate 0.835 :+ negate 0.2321))
        draw BurningShip x y = fractal x y (\z -> (abs (realPart z) :+ abs (imagPart z))**2 + (x:+y))

maxIter :: Double
maxIter = 128

fractal :: Double -> Double -> (Complex Double -> Complex Double) -> Colour
fractal x y frac 
            | iters >= maxIter = black
            | otherwise        = mkSafeHSV iters maxIter
                where
                    iters = divergence (x:+y) 0 frac
                    
divergence :: Complex Double -> Double -> (Complex Double -> Complex Double) -> Double
divergence z n frac = go z n
            where go z' n'
                   | magnitude z' <= 16 && n' < maxIter = go (frac z') (n'+1)
                   | otherwise                         = if isNaN scaledN then maxIter else scaledN
                    where
                        scaledN = n' - (logBase 2 . logBase 2 . sqrt) (magnitude z')
