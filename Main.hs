module Main (main) where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Plant
import Renderer

type World = (Plant, Float, StdGen)

window :: Display
window = InWindow "plant" (800, 600) (240, 100)

viewPort :: ViewPort 
viewPort = ViewPort { viewPortTranslate = (0, -14),
                      viewPortRotate = 0, viewPortScale = 18 }

main :: IO ()
main = newStdGen >>= \g -> let (stem, g') = randomStem g
                           in simulateWorld (stem, 0, g')

playWorld :: World -> IO ()
playWorld = \w -> play window background 60 w drawWorld changeWorld stepWorld 

simulateWorld :: World -> IO ()
simulateWorld w = simulate window background 60 w drawWorld (const stepWorld)

drawWorld :: World -> Picture
drawWorld (a, _, _) = applyViewPortToPicture viewPort $ drawPlant a

changeWorld :: Event -> World -> World
--changeWorld (EventKey (SpecialKey KeySpace) Down _ _) (p, t, g) = (p, t, g')
--  where (p, g') = randomStem 20 g
changeWorld _ world = world

stepWorld :: Float -> World -> World
stepWorld dt w = swayWorld $ growWorld $ mutateWorld $ tick dt w
  where swayWorld (p, t, g)   = (sway p t, t, g)
        growWorld (p, t, g)   = (grow p, t, g)
        mutateWorld (p, t, g) = let (p', g') = mutate p g in (p', t, g')

tick :: Float -> World -> World
tick dt (p, t, g) = (p, t + dt, g)
