module Main where

import Data.VEFC
import Text.ParserCombinators.Parsec (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [shape, fname] -> do
      s <- readFile fname
      case parse pVEFC fname s of
        Left err -> print err
        Right vefc -> putStrLn $ pprint shape vefc
    _ -> putStrLn "Usage: vefc-parser shapeName shape-file.txt"
