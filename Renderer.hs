module Renderer(
    drawPlantZipper,
    drawArrow,
    floralWhite) where

import Graphics.Gloss
import Plant

teaGreen    = makeColorI 196 241 190 255
floralWhite = makeColorI 255 252 242 255
apricot     = makeColorI 255 201 181 255
graniteGray = makeColorI 109 100 102 255

drawPlantZipper :: PlantZipper -> Picture
drawPlantZipper z = pictures [a, b]
  where a = color red $ drawPlant $ fst $ topMost z
        b = drawArrow z
--drawPlantZipper (plant, crumbs) = pictures [ drawPlant plant, t]
--  where t = scale 0.01 0.01 $ text $ show $ length crumbs

drawPlant :: Plant -> Picture
drawPlant Leaf = leaf
--drawPlant (Node length angle (Leaf _) plant) = pictures [stem length, p, join]
--  where p = translate 0 length $ rotate angle $ drawPlant plant
--drawPlant (Node length angle plant (Leaf _)) = pictures [stem length, p, join]
--  where p = translate 0 length $ rotate (-angle) $ drawPlant plant
drawPlant (Node length angle plant1 plant2) = pictures [s1, s2, p1, p2, join]
  where s1 = rotate (-angle) $ stem length
        s2 = rotate ( angle) $ stem length
        p1 = rotate (-angle) $ translate 0 length $ drawPlant plant1
        p2 = rotate ( angle) $ translate 0 length $ drawPlant plant2

drawArrow :: PlantZipper -> Picture
drawArrow (plant, bs) = translate x y $ scale 0.005 0.005 $ text ">"
  where x = -3
        y = fromIntegral $ length bs

leaf :: Picture
leaf = color apricot $ circleSolid 0.25

stem :: Float -> Picture
stem l = color graniteGray $ translate 0 (l / 2) $ rectangleSolid 0.2 l

join :: Picture
join = color graniteGray $ circleSolid 0.1
