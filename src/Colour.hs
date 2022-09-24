module Colour (Colour (..), mkHSV, mkSafeHSV, black, white) where

import Data.Maybe (fromMaybe)

data Colour = RGB !Double !Double !Double 
            | HSV !Double !Double !Double 
            deriving (Eq)

instance Show Colour where
    show (HSV h s v) = maybe "Error" show (clr2rgb (HSV h s v))
    show (RGB r g b) = show r' ++ " " ++ show g' ++ " " ++ show b'
                        where
                            r' = intFloor r
                            g' = intFloor g
                            b' = intFloor b

black :: Colour
black = RGB 0 0 0

white :: Colour
white = RGB 255 255 255

intFloor :: RealFrac a => a -> Int
intFloor = floor :: RealFrac a => a -> Int

mkHSV :: Double -> Double -> Maybe Colour
mkHSV x maxX
        | x <= maxX && maxX <= 0 = Nothing
        | otherwise              = Just $ HSV ((x/maxX) * 359) 1 1

mkSafeHSV :: Double -> Double -> Colour
mkSafeHSV x maxX = fromMaybe white (mkHSV x maxX)

clr2rgb :: Colour -> Maybe Colour
clr2rgb (RGB r g b) = Just $ RGB r g b
clr2rgb (HSV h s v) = case i of
                          0 -> Just $ scaleRGB v t p
                          1 -> Just $ scaleRGB q v p
                          2 -> Just $ scaleRGB p v t
                          3 -> Just $ scaleRGB p q v
                          4 -> Just $ scaleRGB t p v
                          _ -> Just $ scaleRGB v p q
                    where
                        hh                = h / 60
                        i                 = floor hh
                        ff                = hh - fromInteger i
                        p                 = v * (1.0 - s)
                        q                 = v * (1.0 - (s * ff))
                        t                 = v * (1.0 - (s * (1 - ff)))
                        scaleRGB r' g' b' = RGB (r'*254) (g'*254) (b'*254)
