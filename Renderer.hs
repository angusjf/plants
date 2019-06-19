module Renderer where

import Graphics.Gloss
import Plant

teaGreen    = makeColorI 196 241 190 255
floralWhite = makeColorI 255 252 242 255
apricot     = makeColorI 255 201 181 255
graniteGray = makeColorI 109 100 102 255
budGreen    = makeColorI 111 175 100 255
jet         = makeColorI 25 36 45 255
mintyRose   = makeColorI 212 206 201 255
azureMist   = makeColorI 94 154 163 255

background  = floralWhite
leafColor   = apricot
stemColor   = jet

drawPlant :: Plant -> Picture
drawPlant = drawPlant'
{-
drawPlant p = pictures [ t , drawPlant' p ]
  where t = translate (-5) 5 $ scale 0.01 0.01 $ text $ show $ getPlantSize p
  -}

drawPlant' :: Plant -> Picture
drawPlant' Leaf = leaf
drawPlant' (Stem h a nextPlant) = rotate a $ pictures
    [stem h, join, translate 0 h $ drawPlant' nextPlant]
drawPlant' (Fork leftPlant rightPlant) = pictures
    [drawPlant' leftPlant, join, drawPlant' rightPlant]

leaf :: Picture
leaf = color leafColor $ circleSolid 0.25

stem :: Float -> Picture
stem l = color stemColor $ translate 0 (l / 2) $ rectangleSolid 0.2 l

join :: Picture
join = color stemColor $ circleSolid 0.1

getPlantSize :: Plant -> Int
getPlantSize Leaf = 1
getPlantSize (Stem _ _ p) = 1 + getPlantSize p
getPlantSize (Fork p1 p2) = 1 + getPlantSize p1 + getPlantSize p2
