module Data.VEFC where

import Data.List (intercalate)
import Data.Char (toUpper)
import Text.ParserCombinators.Parsec

data VEFC = VEFC
  { vertices :: [Vertex]
  , edges :: [(Int, Int)]
  , faces :: [[Int]]
  , cells :: [[Int]]
  } deriving (Show, Eq)

type Vertex = (Float, Float, Float, Float)

-- Output
pprint :: String -> VEFC -> String
pprint shape@(x:xs) (VEFC v e f _) =
  let vs = printVertices shape v
      es = printEdges shape e
      fs = printFaces shape f
  in unlines $ [ "module Four.Geometry." ++ ((toUpper x) : xs) ++ " where"
               , ""
               , "import Math.Vector4 exposing (..)"
               , "import Array"
               , ""
               ] ++ intercalate [""] [vs, es, fs]

listIndent :: [String] -> [String]
listIndent = map ("  , " ++)

printVertex :: Vertex -> String
printVertex (a,b,c,d) =
  "vec4 " ++ intercalate " " (map show [a,b,c,d])

printVertices :: String -> [Vertex] -> [String]
printVertices shape vs =
  let (v':vs') = map printVertex vs
  in [ shape ++ "Vertices = Array.fromList", "  [ " ++ v' ] ++ listIndent vs' ++ [ "  ]" ]

printEdges :: String -> [(Int,Int)] -> [String]
printEdges shape es =
  let (e':es') = map show es
  in [ shape ++ "Edges =", "  [ " ++ e' ] ++ listIndent es' ++ [ "  ]" ]

printFaces :: String -> [[Int]] -> [String]
printFaces shape fs =
  let (f':fs') = concatMap tris fs
  in [ shape ++ "Faces =", "  [ " ++ f' ] ++ listIndent fs' ++ [ "  ]" ]

tris :: [Int] -> [String]
tris = map show . triGroups

triGroups :: [Int] -> [(Int, Int, Int)]
triGroups face@(x:_) = go x face
  where go a [b,c] = [(b,c,a)]
        go a (a':rest@(b:c:_)) = [(a',b,c)] ++ go a rest
        go a _ = []

-- Parser

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
