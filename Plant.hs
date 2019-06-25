module Plant where

import System.Random

data Plant = Stem Float Float Float Float Plant | Fork Plant Plant | Leaf deriving (Show)

-- -- -- -- GENERATION -- -- -- -- 

--randomPlant :: Int -> StdGen -> (Plant, StdGen)
--randomPlant n g | n > 0 = case randomR (0 :: Int, 2) g of 
--                             --(0, g') -> (Leaf, g')
--                             (1, g') -> randomFork (n - 1) g'
--                             (_, g') -> randomStem (n - 1) g'
--                | otherwise = (Leaf, g)

--randomStem :: Int -> StdGen -> (Plant, StdGen)
--randomStem n g = (Stem h (if leftOrRight then a else (-a)) plant, g'''')
--  where (h, g') = randomR (1, 4) g
--        (leftOrRight, g'') = random g'
--        (a, g''') = randomR (25 :: Float, 55) g''
--        (plant, g'''') = randomPlant (n - 1) g'''

randomStem :: StdGen -> (Plant, StdGen)
randomStem g = (Stem 0 a hmax v Leaf, g'''')
  where (hmax, g') = randomR (1, 4) g
        (leftOrRight, g'') = random g'
        (b, g''') = randomR (10 :: Float, 35) g''
        (v, g'''') = randomR (0.02, 0.04) g'''
        a = (if leftOrRight then b else (-b))

--randomFork :: Int -> StdGen -> (Plant, StdGen)
--randomFork n g = (Fork stem1 stem2, g'')
--  where (stem1, g') = randomStem (n - 1) g
--        (stem2, g'') = randomStem (n - 1) g'

-- -- -- -- SIMULATION -- -- -- -- 

grow :: Plant -> Plant
grow (Stem h a maxH v Leaf) = Stem nextH a maxH v Leaf
  where nextH = h + v
grow (Stem h a maxH v p) = Stem h a maxH v nextP
  where nextP = grow p
grow (Fork p1 p2) = Fork nextP1 nextP2
  where nextP1 = grow p1
        nextP2 = grow p2
grow p = p

sway :: Plant -> Float -> Plant
sway (Stem h a maxH v p) t = Stem h nextA maxH v nextP
  where nextA = stepAngle t a
        nextP = sway p t
        stepAngle t a = a + sin (t * 0.5) * 0.005
sway (Fork p1 p2) t = Fork nextP1 nextP2
  where nextP1 = sway p1 t
        nextP2 = sway p2 t
sway p t = p

mutate :: Plant -> StdGen -> (Plant, StdGen)
mutate Leaf g = (Leaf, g)
mutate (Stem h a maxH v Leaf) g = if h < maxH then (Stem h a maxH v Leaf, g)
    else let (r, g') = randomR (0, 1 :: Double) g
             (next, g'')  = randomStem g'
         in if r < 0.7 then (Stem h a maxH v next, g'')
            else (Stem h a maxH v (Fork (Stem 0 0 maxH v Leaf) next), g'')
mutate (Stem h a maxH v p) g = (Stem h a maxH v newPlant, g')
  where (newPlant, g') = mutate p g
mutate (Fork p1 p2) g = (Fork newP1 newP2, g1')
  where (g1, g2) = split g
        (newP1, g1') = mutate p1 g1
        (newP2, g2') = mutate p2 g2


withSpeed :: Float -> Plant -> Plant
withSpeed v (Stem h a hmax _ Leaf) = Stem h a hmax v Leaf
