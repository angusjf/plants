import Graphics.Gloss

data Plant = Leaf | Node Float Plant Plant

-----------------

window = InWindow "\"PLANT\"" (500, 500) (400, 120)

main :: IO ()
main = visualise $ drawPlant 35 . constructPlantFromString

visualise :: (String -> Picture) -> IO ()
visualise f = do s <- getContents
                 display window white (f s)

-----------------

drawPlant :: Float -> Plant -> Picture
drawPlant angle Leaf = color (dim green) (circle 0.1)
drawPlant angle (Node length p1 p2) = pictures [stem, plant1, plant2]
  where
    stem = line' [(0, 0), (0, length)]
    transform theta = translate 0 length . rotate theta . drawPlant theta
    plant1 = transform newAngle    p1
    plant2 = transform (-newAngle) p2
    newAngle = angle / 1.1

line' :: Path -> Picture
line' path = pictures $ line path : map f path
  where f (a, b) = color red $ translate a b (circle 0.1)

lerp :: Float -> Float -> Float -> Float
lerp a b t = a * (1 - t) + b * t

------------------------------------------------------------------------

xlPlant :: Plant
xlPlant = Node 6 largePlant (Node 4 mediumPlant (Node 3 Leaf Leaf))

largePlant :: Plant
largePlant = Node 5 smallPlant mediumPlant

mediumPlant :: Plant
mediumPlant = Node 4
    (Node 3 Leaf (Node 2 smallPlant (Node 1 (Node 1 Leaf Leaf) Leaf)))
    smallPlant

smallPlant :: Plant
smallPlant = Node 3
    (Node 2 (Node 2 Leaf Leaf) Leaf)
    (Node 1 Leaf Leaf)

------------------------------------------------------------------------

constructPlantFromString :: String -> Plant
constructPlantFromString s = constructPlantFromLexemes (words s)

data PlantData = Fork Float PlantData PlantData | String

stringsToPlantData :: [String] -> PlantData
stringsToPlantData [x]         = "l"
stringsToPlantData [a, b, c]   = (a, b,   c)
stringsToPlantData a:"l":xs    = (a, "l", stringsToPlantData xs
stringsToPlantData 
