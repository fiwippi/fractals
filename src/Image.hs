module Image (Image, drawImage, saveImage) where

import Colour (Colour)

data Image = Image {
      width  :: !Int
    , height :: !Int
    , pixels :: [[Colour]]
    }

drawImage :: Int -> Int -> (Double -> Double -> Colour) -> Double -> Double -> Image
drawImage w h draw zoom offset = Image{width=w, height=h, pixels=px}
                where
                    px      = [[draw (adj x w) (adj y h) | x <- [1..w]] | y <- [1..h]]
                    adj a b = zoom * (fromIntegral a / fromIntegral b - offset)

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
