module Data.VEFCSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec (parse, eof)
import Data.VEFC

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pVertex" $ do
    it "should parse a single vertex" $ do
      parse pVertex "" "1, 0, 0, 0\n" `shouldBe` Right (1, 0, 0, 0)
  describe "pVertices" $ do
    it "should parse a series of vertices" $ do
      parse pVertices "" "1, 0, 0, 0\n2.5, -1.5, 3, 6\n" `shouldBe` Right [ (1,0,0,0)
                                                                          , (2.5, -1.5, 3, 6)]
