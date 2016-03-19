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
  describe "triGroups" $ do
    it "should return 1 triplets for [1,2,3]" $
      triGroups [1,2,3] `shouldBe` [(1,2,3)]
    it "should return 2 triplets for [1,2,3,4]" $
      triGroups [1,2,3,4] `shouldBe` [(1,2,3), (3,4,1)]
    it "should return 3 triplets for [1,2,3,4,5]" $
      triGroups [1,2,3,4,5] `shouldBe` [(1,2,3), (3,4,1), (4,5,1)]
