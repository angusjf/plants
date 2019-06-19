module Main (main) where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Plant
import Renderer

type World = (Plant, Float, StdGen)

window :: Display
window = InWindow "\"PLANT\"" (500, 500) (400, 120)

viewPort :: ViewPort 
viewPort = ViewPort { viewPortTranslate = (0, -12)
                    , viewPortRotate = 0
                    , viewPortScale = 18 }

main :: IO ()
main = do g <- newStdGen
          playWorld (a, 0, g)
            where a = Stem 10 10 (Fork (Stem 3 (-10) (Fork (Stem 1 (-10) Leaf) (Stem 1 10 Leaf)))
                                       (Stem 3 10    (Fork (Stem 1 (-10) Leaf) (Stem 1 10 Leaf))))

randomPlant :: StdGen -> (Plant, StdGen)
randomPlant g = case randomR (0 :: Int, 2) g of 
                  (0, g') -> (Leaf, g')
                  (1, g') -> randomFork g'
                  (2, g') -> randomStem g'

randomStem :: StdGen -> (Plant, StdGen)
randomStem g = (Stem h a plant, g''')
  where (h, g') = randomR (0.5, 4) g
        (a, g'') = randomR ((-45) :: Float, 45) g'
        (plant, g''') = randomPlant g''

randomFork :: StdGen -> (Plant, StdGen)
randomFork g = (Fork stem1 stem2, g'')
  where (stem1, g') = randomStem g
        (stem2, g'') = randomStem g'

playWorld :: World -> IO ()
playWorld = \w -> play window background 60 w drawWorld changeWorld stepWorld 

drawWorld :: World -> Picture
drawWorld (a, b, c) = applyViewPortToPicture viewPort $ drawPlant a

changeWorld :: Event -> World -> World
changeWorld (EventKey (SpecialKey KeySpace) Down _ _) (p, t, g) = (newPlant, t, newG)
  where (newPlant, newG) = randomPlant g
        
changeWorld _ world = world

stepWorld :: Float -> World -> World
stepWorld dt (Leaf, t, g) = (Leaf, t + dt, g)
stepWorld dt (Stem h a Leaf, t, g) = (nextStem, t + dt, g')
  where (v, g') = randomR (0 :: Float, 120) g
        nextStem = if v > 119 then stemWithFork else longerStem
        longerStem = Stem (h + 0.004) a Leaf
        stemWithFork = Stem h a (Fork (Stem 1 15 Leaf) (Stem 1 (-15) Leaf))
stepWorld dt (Stem h a p, t, g) = (Stem h a newPlant, t + dt, g')
  where (newPlant, _, g') = stepWorld dt (p, t, g)
stepWorld dt (Fork p1 p2, t, g) = (Fork newP1 newP2, t + dt, g1')
  where (g1, g2) = split g
        (newP1, _, g1') = stepWorld dt (p1, t, g1)
        (newP2, _, g2') = stepWorld dt (p2, t, g2)
