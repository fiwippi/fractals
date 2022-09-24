module Main (main) where

import Test.HUnit

import Colour (Colour(HSV))

-- Colour
hsv2rgb = TestLabel "HSV to RGB" $ TestList [ testColour "White"   "255 255 255" 0   0 1
                                            , testColour "Black"   "0 0 0"       0   0 0
                                            , testColour "Red"     "255 0 0"     0   1 1
                                            , testColour "Lime"    "0 255 0"     120 1 1
                                            , testColour "Blue"    "0 0 255"     240 1 1
                                            , testColour "Yellow"  "255 255 0"   60  1 1
                                            , testColour "Cyan"    "0 255 255"   180 1 1
                                            , testColour "Magenta" "255 0 255"   300 1 1
                                            , testColour "Silver"  "191 191 191" 0   0 0.75
                                            , testColour "Grey"    "128 128 128" 0   0 0.5
                                            , testColour "Maroon"  "128 0 0"     0   1 0.5
                                            , testColour "Olive"   "128 128 0"   60  1 0.5
                                            , testColour "Green"   "0 128 0"     120 1 0.5
                                            , testColour "Purple"  "128 0 128"   300 1 0.5
                                            , testColour "Teal"    "0 128 128"   180 1 0.5
                                            , testColour "Navy"    "0 0 128"     240 1 0.5
                                            ] 

testColour :: String -> String -> Double -> Double -> Double -> Test
testColour name rgb h s v = TestCase $ assertEqual
        name rgb (show $ HSV h s v)

main :: IO ()
main = runTestTTAndExit $ TestList [hsv2rgb]
