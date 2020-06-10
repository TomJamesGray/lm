module Lm.Matrix.MatMultSpec (spec) where

import Lm.Matrix
import Test.Hspec

spec :: Spec
spec = do
  describe "Matrix multiplication" $ do
    it "Tests multiplication with identity" $ do
      let a = matFromList [[1,1,1],[1,1,1],[1,1,1]]
          b = idMat 3
        in (mProduct a b) `shouldBe` a

    it "Tests simple product" $ do
      let
        a = matFromList [[1,2],[3,4]]
        b = matFromList [[-2,3],[10,6]]
        in (mProduct a b) `shouldBe` (matFromList [[18,15],[34,33]])