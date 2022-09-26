module Image (Image, drawImage, saveImage) where

import Colour (Colour)
import Drawing (Drawer(draw))

data Image = Image {
      width  :: !Int
    , height :: !Int
    , pixels :: ![[Colour]]
    }

(//) :: Int -> Int -> Double
(//) a b = fromIntegral a / fromIntegral b
infixl 7 //

drawImage :: (Drawer a) => Int -> Int -> a -> Double -> Double -> Image
drawImage w h drawer zoom offset = Image{width=w, height=h, pixels=px}
                where
                    px      = [[draw drawer (adj x w) (adj y h) | x <- [1..w]] | y <- [1..h]]
                    adj a b = ((a // b) * zoom * 2) + (offset - zoom)

serialise :: [[Colour]] -> String
serialise []     = []
serialise (r:rs) = serialiseRow r ++ serialise rs
                where
                    serialiseRow :: [Colour] -> String
                    serialiseRow [] = "\n"  
                    serialiseRow (c:cs)  
                        | null cs   = show c ++ serialiseRow cs
                        | otherwise = show c ++ " " ++ serialiseRow cs

saveImage :: Image -> FilePath -> IO ()
saveImage img fp = writeFile fp ppm
                where
                    ppm = "P3\n" ++ w ++ " " ++ h ++ "\n255\n" ++ px
                    w  = show $ width img
                    h  = show $ height img
                    px = serialise (pixels img)
