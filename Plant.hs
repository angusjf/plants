module Plant ( Plant(..)
             , PlantZipper
             , modifyWholePlant
             , topMost
             , selectLeft
             , selectRight
             , extendPlant
             , goUp
             , stepPlant ) where

data Plant = Leaf | Node Float Float Plant Plant deriving (Show)
data Crumb = LeftCrumb Float Float Plant | RightCrumb Float Float Plant
type PlantZipper = (Plant, [Crumb])

modifyWholePlant :: (Plant -> Plant) -> PlantZipper -> PlantZipper
modifyWholePlant f (plant, bs) = (f plant, map (modifyCrumb f) bs)

modifyCrumb :: (Plant -> Plant) -> Crumb -> Crumb
modifyCrumb f (LeftCrumb h a plant) = LeftCrumb h a (f plant)
modifyCrumb f (RightCrumb h a plant) = RightCrumb newH newA (f plant)
  where (Node newH newA _ _) = f (Node h a Leaf Leaf)

topMost :: PlantZipper -> PlantZipper
topMost (plant, []) = (plant, [])
topMost z = topMost $ goUp z

goUp :: PlantZipper -> PlantZipper
goUp (plant, LeftCrumb h a r:rest)  = (Node h a plant r, rest)
goUp (plant, RightCrumb h a l:rest) = (Node h a l plant, rest)

goLeft :: PlantZipper -> PlantZipper
goLeft (Node h a l r, bs) = (l, LeftCrumb h a r:bs)

goRight :: PlantZipper -> PlantZipper
goRight (Node h a l r, bs) = (r, RightCrumb h a l:bs)

selectLeft :: PlantZipper -> PlantZipper
--selectLeft (plant, ds) = (plant, ds)
selectLeft = goLeft

selectRight :: PlantZipper -> PlantZipper
--selectRight (plant, ds) = (plant, ds)
selectRight = goRight

extendPlant :: PlantZipper -> PlantZipper
extendPlant (Leaf, bs) = (Node h a Leaf Leaf, bs)
  where h = 1
        a = 25
extendPlant a = a

stepPlant :: Float -> Plant -> Plant
stepPlant t Leaf             = Leaf
stepPlant t (Node h a p1 p2) = Node h (stepAngle t a) (stepPlant t p1) (stepPlant t p2)
  where stepAngle t a = a + sin (t * 0.5) * 0.05
