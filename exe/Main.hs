module Main (main) where

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Colour (Colour)
import Fractal (mandelbrot, julia)
import Image (drawImage, saveImage)

exit :: IO ()
exit = do 
          putStrLn $ "Available fractals: " ++ show (intercalate ", " fractals)
          putStrLn ""
          putStrLn "Usage: ./fractals width height zoom offset fractal filepath"
          putStrLn "       ./fractals 600 600 4.5 0 mandelbrot fractal.ppm"
          exitWith $ ExitFailure 1

fractals :: [String]
fractals = ["mandelbrot", "julia"]

getFractal :: String -> (Double -> Double -> Colour)
getFractal f = case f of
                   "mandelbrot" -> mandelbrot
                   "julia"      -> julia
                   _            -> error "invalid fractal"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [w, h, z, off, frac, fp] -> if null fp || frac `notElem` fractals || Nothing `elem` [w', h'] || Nothing `elem` [z', off']
                                        then exit 
                                    else saveImage (drawImage (fj w') (fj h') (getFractal frac) (fj z') (fj off')) fp 
                                    where w'   = readMaybe w :: Maybe Int
                                          h'   = readMaybe h :: Maybe Int
                                          z'   = readMaybe z :: Maybe Double
                                          off' = readMaybe off :: Maybe Double
                                          fj   = fromJust
        _         -> exit

