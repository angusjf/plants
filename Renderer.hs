module Renderer where

import Graphics.Gloss
import Plant

teaGreen    = makeColorI 196 241 190 255
floralWhite = makeColorI 255 252 242 255
apricot     = makeColorI 255 201 181 255
graniteGray = makeColorI 109 100 102 255
budGreen    = makeColorI 111 175 100 255
background = floralWhite

drawPlant :: Plant -> Picture
drawPlant Leaf = leaf
drawPlant (Stem h a nextPlant) = rotate a $ pictures
    [stem h, translate 0 h $ drawPlant nextPlant]
drawPlant (Fork leftPlant rightPlant) = pictures
    [drawPlant leftPlant, join, drawPlant rightPlant]

leaf :: Picture
leaf = color apricot $ circleSolid 0.25

stem :: Float -> Picture
stem l = color graniteGray $ translate 0 (l / 2) $ rectangleSolid 0.2 l

join :: Picture
join = color graniteGray $ circleSolid 0.1
