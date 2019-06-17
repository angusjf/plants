module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Plant
import Renderer

window :: Display
window = InWindow "\"PLANT\"" (500, 500) (400, 120)

viewPort :: ViewPort 
viewPort = ViewPort { viewPortTranslate = (0, -12)
                    , viewPortRotate = 0
                    , viewPortScale = 18 }

type World = (PlantZipper, Float)

main :: IO ()
main = play window floralWhite 60 initialWorld draw changeWorld stepWorld 
  where draw = applyViewPortToPicture viewPort . drawPlantZipper . fst
        initialWorld = ((initialPlant, []), 0)
        initialPlant = Node 2 25 (Node 1 25 Leaf Leaf) (Node 1 25 Leaf Leaf)

changeWorld :: Event -> World -> World
changeWorld (EventKey (SpecialKey KeySpace) Down _ _) (z, t) = (extendPlant z, t)
changeWorld (EventKey (SpecialKey KeyLeft)  Down _ _) (z, t) = (selectLeft z, t)
changeWorld (EventKey (SpecialKey KeyDown)  Down _ _) (z, t) = (goUp z, t)
changeWorld (EventKey (SpecialKey KeyRight) Down _ _) (z, t) = (selectRight z, t)
changeWorld _ world = world

stepWorld :: Float -> World -> World
stepWorld dt (z, t) = (modifyWholePlant (stepPlant t) z, t + dt)
