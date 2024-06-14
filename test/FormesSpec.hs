module FormesSpec (spec) where 

import Test.Hspec
import Formes
import Types 

spec :: Spec
spec = do
  describe "limites" $ do
    it "calculates correct limits for a horizontal segment" $
      limites (HSegment (C 5 10) 3) `shouldBe` (10, 10, 5, 7)

    it "calculates correct limits for a vertical segment" $
      limites (VSegment (C 5 10) 4) `shouldBe` (10, 13, 5, 5)

    it "calculates correct limits for a rectangle" $
      limites (Rectangle (C 3 4) 6 5) `shouldBe` (4, 8, 3, 8)

  describe "appartient" $ do
    it "returns True if coord is within a horizontal segment" $
      appartient (C 6 10) (HSegment (C 5 10) 3) `shouldBe` True

    it "returns False if coord is outside a horizontal segment" $
      appartient (C 8 10) (HSegment (C 5 10) 3) `shouldBe` False

    it "returns True if coord is within a vertical segment" $
      appartient (C 5 12) (VSegment (C 5 10) 4) `shouldBe` True

    it "returns True if coord is within a rectangle" $
      appartient (C 4 5) (Rectangle (C 3 4) 6 5) `shouldBe` True

    it "returns False if coord is outside a rectangle" $
      appartient (C 10 10) (Rectangle (C 3 4) 6 5) `shouldBe` False

  describe "collision_approx" $ do
    it "returns True when two rectangles overlap" $
      collision_approx (Rectangle (C 1 1) 5 5) (Rectangle (C 4 4) 3 3) `shouldBe` True

    it "returns False when two rectangles do not overlap" $
      collision_approx (Rectangle (C 1 1) 2 2) (Rectangle (C 4 4) 3 3) `shouldBe` False

    it "returns True when a rectangle and a horizontal segment overlap" $
      collision_approx (Rectangle (C 1 1) 10 10) (HSegment (C 5 5) 3) `shouldBe` True

    it "returns False when a rectangle and a vertical segment do not overlap" $
      collision_approx (Rectangle (C 1 1) 2 2) (VSegment (C 10 10) 5) `shouldBe` False