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

type World = (Plant, Float)

main :: IO ()
main = play window floralWhite 60 initialWorld draw changeWorld stepWorld 
  where draw = applyViewPortToPicture viewPort . drawPlant . fst
        initialWorld = (initialPlant, 0)
        initialPlant = Node 1 15 (Leaf False) (Leaf True)

changeWorld :: Event -> World -> World
changeWorld (EventKey (SpecialKey KeySpace)     Down _ _) (plant, t) = (extendPlant plant, t)
changeWorld (EventKey (SpecialKey KeyBackspace) Down _ _) (plant, t) = (selectRoot plant, t)
changeWorld (EventKey (SpecialKey KeyLeft)      Down _ _) (plant, t) = (selectLeft plant, t)
changeWorld (EventKey (SpecialKey KeyRight)     Down _ _) (plant, t) = (selectRight plant, t)
changeWorld _ world = world

stepWorld :: Float -> World -> World
stepWorld dt (plant, t) = (stepPlant t plant, t + dt)

--  initialPlant = (Node 2 35 (Node 2 25 (Node 1 15 (Node 1 15 (Node 1 15 (Node 1 15 (Leaf False) (Leaf False)) (Node 1 15 (Node 1 15 (Leaf False) (Leaf False)) (Leaf False))) (Node 1 15 (Leaf False) (Node 1 15 (Leaf False) (Leaf False)))) (Leaf False)) (Node 1 15 (Leaf False) (Leaf True))) (Node 1 15 (Leaf False) (Leaf False)))
