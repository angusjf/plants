import Data.Char
import Text.ParserCombinators.ReadP

data Plant = Leaf | Node Int Plant Plant deriving (Show)

parsePlant :: ReadP Plant
parsePlant = do
  plant <- parseLeaf +++ parseNode
  eof
  return plant

parseInt :: ReadP Int
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
