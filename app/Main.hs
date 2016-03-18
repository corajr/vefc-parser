module Main where

import Data.VEFC
import Text.ParserCombinators.Parsec (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      s <- readFile fname
      print $ parse pVEFC fname s
