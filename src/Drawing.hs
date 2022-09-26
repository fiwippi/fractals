module Drawing (Drawer(draw)) where

import Colour (Colour)

class Drawer a where
        draw :: a -> Double -> Double -> Colour
