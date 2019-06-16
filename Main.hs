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

----------main :: IO ()
-----main = visualise $ applyViewPortToPicture vp . plantPicture . stringToPlant
-----main = visualise (\t -> plantPicture . stringToPlant)
-----  where plantPicture = dnnrawPlant 35
-----
-----visualise :: String -> Float -> Picture -> IO ()
-----visualise f = do s <- getContents
----------                 animate window floralWhite (f s)

main :: IO ()
main = do inputString <- getContents
          putStrLn $ show (stringToPlant inputString)
          animate window floralWhite
            (\time -> drawPlant 35 (stringToPlant inputString) time)

drawPlant :: Float -> Plant -> Float -> Picture

drawPlant angle Leaf                        t = leaf

drawPlant angle (Node length Leaf   plant ) t = pictures [stem length, p, join]
  where
    p = translate 0 length $ rotate   newAngle  $ drawPlant newAngle plant t
    newAngle = angle / (1 + (sin (t * 0.75) + 1) / 60)

drawPlant angle (Node length plant  Leaf  ) t = pictures [stem length, p, join]
  where
    p = translate 0 length $ rotate (-newAngle) $ drawPlant newAngle plant t
    newAngle = angle / (1 + (sin (t * 0.75) + 1) / 60)

drawPlant angle (Node length plant1 plant2) t = pictures [stem length, p1, p2, join]
  where
    p1 = translate 0 length $ rotate (-newAngle) $ drawPlant newAngle plant1 t
    p2 = translate 0 length $ rotate   newAngle  $ drawPlant newAngle plant2 t
    newAngle = angle / (1 + (sin (t * 0.75) + 1) / 60)

leaf :: Picture
leaf = color apricot $ circleSolid 0.25

stem :: Float -> Picture
stem l = color graniteGray $ translate 0 (l / 2) $ rectangleSolid 0.2 l

join :: Picture
join = color graniteGray $ circleSolid 0.1
