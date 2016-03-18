module Data.VEFC where

import Text.ParserCombinators.Parsec

data VEFC = VEFC
  { vertices :: [Vertex]
  , edges :: [(Int, Int)]
  , faces :: [[Int]]
  , cells :: [[Int]]
  } deriving (Show, Eq)

type Vertex = (Float, Float, Float, Float)

pFloat :: GenParser Char st Float
pFloat = read <$> many (choice [ digit, char '.', char '-' ] )

pInt :: GenParser Char st Int
pInt = read <$> many1 digit

cSep = string ", "

pVertex = do
  coords <- (pFloat `sepBy` cSep)
  newline
  case coords of
   [a,b,c,d] -> return (a, b, c, d)
   _ -> fail "no"

pVertices = many pVertex

pEdge = do
  a <- pInt
  cSep
  b <- pInt
  newline
  return (a, b)

pIndices = (pInt `sepBy1` cSep) <* optional newline

pVEFC = do
  string "V\n"
  v <- pVertices
  string "E\n"
  e <- many pEdge
  string "F\n"
  f <- many pIndices
  string "C\n"
  c <- many pIndices
  eof
  return $ VEFC v e f c
