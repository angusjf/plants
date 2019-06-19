module Plant where

data Plant = Stem Float Float Plant | Fork Plant Plant | Leaf deriving (Show)

extendPlant :: Plant -> Plant
extendPlant p = Stem 1 0 Leaf

addFork :: Plant -> Plant
addFork (Stem h a plant) = Stem h a (Fork Leaf Leaf)
addFork p = p

stepPlant :: Float -> Plant -> Plant
stepPlant t Leaf             = Leaf
stepPlant t (Stem h a plant) = Stem h (stepAngle t a) (stepPlant t plant)
  where stepAngle t a = a + sin (t * 0.5) * 0.05
stepPlant t (Fork p1 p2) = Fork (stepPlant t p1) (stepPlant t p2)
