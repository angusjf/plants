module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import PlantParser

teaGreen = makeColorI 196 241 190 255
floralWhite = makeColorI 255 252 242 255
apricot = makeColorI 255 201 181 255
graniteGray = makeColorI 109 100 102 255

window :: Display
window = InWindow "\"PLANT\"" (500, 500) (400, 120)

vp :: ViewPort 
vp = ViewPort { viewPortTranslate = (0, -8)
              , viewPortRotate = 0
              , viewPortScale = 16 }

main :: IO ()
main = visualise $ applyViewPortToPicture vp . plantPicture . stringToPlant
  where plantPicture = drawPlant 35

visualise :: (String -> Picture) -> IO ()
visualise f = do s <- getContents
                 display window floralWhite (f s)

drawPlant :: Float -> Plant -> Picture
drawPlant angle Leaf = color apricot (circleSolid 0.25)
drawPlant angle (Node length p1 p2) = pictures [stem, plant1, plant2]
  where
    stem = color graniteGray . translate 0 (length / 2) $
        rectangleSolid 0.2 length
    transform theta = translate 0 length . rotate theta . drawPlant theta
    plant1 = id $ transform newAngle    p1
    plant2 = id $ transform (-newAngle) p2
    newAngle = angle / 1.1

--line' :: Path -> Picture
--line' path = pictures $ line path : map f path
--  where f (a, b) = color red $ translate a b (circle 0.1)

--lerp :: Float -> Float -> Float -> Float
--lerp a b t = a * (1 - t) + b * t
