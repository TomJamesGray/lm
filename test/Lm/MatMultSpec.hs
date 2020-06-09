module Lm.MatMultSpec (spec) where

import Lm.Matrix
import Test.Hspec

spec :: Spec
spec = do
  describe "Matrix multiplication" $ do
    it "Tests matrix multiplication" $ do
      (Matrix [[1,0],[0,1]] 2 2)  `shouldBe` (idMat 2)