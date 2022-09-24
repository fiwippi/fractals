module Main where

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Colour (Colour)
import Image (drawImage, saveImage)
import Fractal (mandelbrot, julia, burningShip, fractals)

exit :: IO ()
exit = do 
          putStrLn $ "Available fractals: " ++ show (intercalate ", " fractals)
          putStrLn ""
          putStrLn "Usage: ./fractals width height zoom offset fractal filepath"
          putStrLn "       ./fractals 600 600 4.5 0 mandelbrot fractal.ppm"
          exitWith $ ExitFailure 1

getFractal :: String -> (Double -> Double -> Colour)
getFractal f = case f of
                   "mandelbrot"  -> mandelbrot
                   "julia"       -> julia
                   "burningShip" -> burningShip
                   _             -> error "invalid fractal"

validArgs :: Maybe Int -> Maybe Int -> Maybe Double -> Maybe Double -> String -> String -> Bool
validArgs w h z off frac fp
                        | null fp                 = False
                        | frac `notElem` fractals = False
                        | Nothing `elem` [w, h]   = False
                        | Nothing `elem` [z, off] = False
                        | otherwise               = True

main :: IO ()
main = do
    args <- getArgs
    case args of
        [w, h, z, off, frac, fp] -> if not $ validArgs w' h' z' off' frac fp
                                        then exit 
                                    else saveImage (drawImage (fj w') (fj h') (getFractal frac) (fj z') (fj off')) fp 
                                    where w'   = readMaybe w :: Maybe Int
                                          h'   = readMaybe h :: Maybe Int
                                          z'   = readMaybe z :: Maybe Double
                                          off' = readMaybe off :: Maybe Double
                                          fj   = fromJust
        _failure                 -> exit

