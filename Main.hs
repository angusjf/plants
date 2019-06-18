module Main (main) where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Plant
import Renderer

type World = (Plant, Float)

window :: Display
window = InWindow "\"PLANT\"" (500, 500) (400, 120)

viewPort :: ViewPort 
viewPort = ViewPort { viewPortTranslate = (0, -12)
                    , viewPortRotate = 0
                    , viewPortScale = 18 }

main :: IO ()
main = do g <- newStdGen
          playPlant $ randomStem g

randomPlant :: StdGen -> Plant
randomPlant g = case randomR (0 :: Int, 2) g of 
                  (0, g) -> Leaf
                  (1, g) -> randomFork g
                  (2, g) -> randomStem g


randomStem :: StdGen -> Plant
randomStem g = Stem h a (randomPlant g'')
  where (h, g') = randomR (0.5, 4) g
        (a, g'') = randomR ((-45) :: Float, 45) g'


randomFork :: StdGen -> Plant
randomFork g = Fork (randomStem g1) (randomStem g2)
                 where (g1, g2) = split g

getPlantSize :: Tree -> Int

playPlant :: Plant -> IO ()
playPlant = \p -> play window background 60 (p, 0) drawWorld changeWorld stepWorld 

drawWorld :: World -> Picture
drawWorld = applyViewPortToPicture viewPort . drawPlant . fst

changeWorld :: Event -> World -> World
--changeWorld (EventKey (SpecialKey KeyUp) Down _ _) (z, t) = (extendPlant z, t)
changeWorld (EventKey (SpecialKey KeySpace) Down _ _) (z, t) = (addFork z, t)
--changeWorld (EventKey (SpecialKey KeyLeft) Down _ _) (z, t) = (selectLeft z, t)
--changeWorld (EventKey (SpecialKey KeyDown) Down _ _) (z, t) = (goUp z, t)
--changeWorld (EventKey (SpecialKey KeyRight) Down _ _) (z, t) = (selectRight z, t)
changeWorld _ world = world

stepWorld :: Float -> World -> World
stepWorld dt (z, t) = (stepPlant t z, t + dt)
