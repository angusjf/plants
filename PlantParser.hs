module PlantParser (stringToPlant, Plant(..)) where

import Data.Char
import Text.ParserCombinators.ReadP

data Plant = Leaf | Node Float Plant Plant deriving (Show)

stringToPlant :: String -> Plant
stringToPlant = fst . head . readP_to_S parsePlant 

parsePlant :: ReadP Plant
parsePlant = do
  plant <- parseLeaf +++ parseNode
  return plant

parseInt :: ReadP Float
parseInt = pure read <*> (many1 $ satisfy isDigit)

parseLeaf :: ReadP Plant
parseLeaf = do
  char 'l'
  char ' '
  return Leaf

parseNode :: ReadP Plant
parseNode = do
  char 'f'
  length <- parseInt
  char ' '
  p1 <- parsePlant
  p2 <- parsePlant
  return $ Node length p1 p2
