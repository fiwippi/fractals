module Main where

import Options.Applicative
import Text.Read (readMaybe)
import Data.List (intercalate)

import Fractal (Fractal, fractals)
import Image (drawImage, saveImage)

ensureMin :: (Num a, Ord a, Read a) => a -> String -> Maybe a
ensureMin mn x = case readMaybe x of
                    Nothing -> Nothing
                    Just n  -> if n < mn then Nothing else Just n

main :: IO ()
main = do
        args <- execParser $ info (cmdArgs <**> helper) fullDesc
        let w  = width args
        let h  = height args
        let f  = fractal args
        let z  = zoom args
        let o  = offset args
        let fp = filepath args
        saveImage (drawImage w h f z o) fp

data Args = Args
  { width    :: !Int
  , height   :: !Int
  , zoom     :: !Double
  , offset   :: !Double
  , fractal  :: !Fractal 
  , filepath :: !String }

cmdArgs :: Parser Args
cmdArgs = Args
      <$> option (maybeReader $ ensureMin 1) 
           ( short 'w' <> metavar "WIDTH" <> help "Width of the output image" <>
             showDefault <> value 500 )
      <*> option (maybeReader $ ensureMin 1) 
           ( short 'h' <> metavar "HEIGHT" <> help "Height of the output image" <> 
             showDefault <> value 500 ) 
      <*> option (maybeReader $ ensureMin 0) 
           ( short 'z' <> metavar "ZOOM" <> help "Strength of zoom-out from the image" <> 
             showDefault <> value 1 )
      <*> option auto
           ( short 'o' <> metavar "OFFSET" <> help "Offset of the fractal" <> 
             showDefault <> value 0 )
      <*> option auto
          ( short 'f' <> metavar "FRACTAL" <> 
            help ("Name of the fractal to draw: \"" ++ intercalate ", " (map show fractals) ++ "\"") )
      <*> argument str (metavar "FILE") 
