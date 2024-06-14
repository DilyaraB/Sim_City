module ZonesSpec (spec) where 

import Test.Hspec
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck

import Zones
import Types
import Formes

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe


-- ------INSTANCES ARBITRARY-------

-- instance Arbitrary Coord where
--     arbitrary = C <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)


-- instance Arbitrary Forme where
--     arbitrary = oneof [
--         HSegment <$> arbitrary <*> (getPositive <$> arbitrary),
--         VSegment <$> arbitrary <*> (getPositive <$> arbitrary),
--         Rectangle <$> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary) ]

-- instance Arbitrary Batiment where
--     arbitrary = oneof [
--         Cabane <$> arbitrary <*> (arbitrary >>= genAdjacentCoord) <*> (getPositive <$> arbitrary) <*> arbitrary,
--         Atelier <$> arbitrary <*> (arbitrary >>= genAdjacentCoord) <*> (getPositive <$> arbitrary) <*> arbitrary,
--         Epicerie <$> arbitrary <*> (arbitrary >>= genAdjacentCoord) <*> (getPositive <$> arbitrary) <*> arbitrary,
--         Commissariat <$> arbitrary <*> (arbitrary >>= genAdjacentCoord) ]


-- instance Arbitrary Zone where
--     arbitrary = oneof [
--         Eau <$> arbitrary,
--         Vide <$> arbitrary,
--         Route <$> arbitrary,
--         ZR <$> arbitrary <*> arbitrary,
--         ZI <$> arbitrary <*> arbitrary,
--         ZC <$> arbitrary <*> arbitrary,
--         Admin <$> arbitrary <*> arbitrary ]

-- instance Arbitrary ZoneId where
--     arbitrary = ZoneId <$> arbitrary

-- instance Arbitrary CitId where
--     arbitrary = CitId <$> arbitrary

-- instance Arbitrary BatId where
--     arbitrary = BatId <$> arbitrary

-- instance Arbitrary Citoyen where
--     arbitrary = oneof [
--         Immigrant <$> arbitrary <*> arbitrary <*> arbitrary,
--         Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
--         Emigrant <$> arbitrary <*> arbitrary ]

-- instance Arbitrary Occupation where
--     arbitrary = elements [Travail, Dodo, Courses, Deplacement]

-- instance Arbitrary Ville where
--     arbitrary = Ville <$> arbitrary <*> arbitrary


spec :: Spec
spec = do
  describe "zoneForme" $ do
    it "extracts the Forme from a Zone" $ do
      zoneForme (Eau (Rectangle (C 1 1) 2 2)) `shouldBe` Rectangle (C 1 1) 2 2

  describe "formeToCoords" $ do
    it "produce coordinates for HSegment" $ do
      formeToCoords (HSegment (C 1 1) 3) `shouldMatchList` [C 1 1, C 2 1, C 3 1, C 4 1]
  
  describe "invariants_ville" $ do
    it "ensures that no two zones overlap" $ do
      let nonOverlappingVille = Ville (Map.fromList [
            (ZoneId 1, Route (Rectangle (C 1 1) 10 10)),
            (ZoneId 2, Route (Rectangle (C 12 12) 10 10))]) Map.empty
      invariant_disjoint_zones nonOverlappingVille `shouldBe` True

    it "ensures that no two zones overlap" $ do
      let overlappingVille = Ville (Map.fromList [
            (ZoneId 1, Route (Rectangle (C 1 1) 10 10)),
            (ZoneId 2, Route (Rectangle (C 5 5) 10 10))]) Map.empty
      invariant_disjoint_zones overlappingVille `shouldBe` False

    it "ensures that all residential, commercial, industrial, or administrative zones are adjacent to at least one road zone" $ do
      let wellConnectedVille = Ville (Map.fromList [
            (ZoneId 1, Route (VSegment (C 0 0) 10)),
            (ZoneId 2, ZR (Rectangle (C 1 1) 2 2) [])]) Map.empty
      invariant_adjacente_a_route wellConnectedVille `shouldBe` True

    it "checks that all routes are connected" $ do
      let connectedVille = Ville (Map.fromList [
            (ZoneId 1, Route (Rectangle (C 0 0) 10 1)),
            (ZoneId 2, Route (Rectangle (C 0 1) 10 1)) ]) Map.empty
      invariant_routes_connexes connectedVille `shouldBe` True

  describe "Construction of new city" $ do
    it "constructs a new city with additional zone" $ do
      let initialVille = Ville (Map.fromList [
            (ZoneId 1, Route (HSegment (C 0 0) 10)),  -- Route from (0,0) to (10,0)
            (ZoneId 2, Route (HSegment (C 10 0) 5))]) Map.empty
      let newZone = ZR (Rectangle (C 5 1) 3 2) []
      let result = construit initialVille newZone
      result `shouldSatisfy` isJust

    it "fails to construct a new city due to overlap" $ do
      let ville = Ville (Map.fromList [(ZoneId 1, Route (Rectangle (C 1 1) 10 10))]) Map.empty
      let overlappingZone = Route (Rectangle (C 1 1) 5 5)
      let result = construit ville overlappingZone
      result `shouldBe` Nothing
  
  describe "Complex construction of a new city" $ do
    it "constructs a complex city with multiple zones" $ do
      let initialVille = Ville (Map.fromList [
            (ZoneId 1, Route (HSegment (C 0 0) 10)),  
            (ZoneId 2, Route (VSegment (C 10 0) 10)),
            (ZoneId 3, Eau (Rectangle (C 0 10) 3 3)), -- Water zone, does not need adjacency to routes
            (ZoneId 4, Admin (Rectangle (C 5 1) 2 2) (Cabane (BatId "bat1") (Rectangle (C 5 1) 2 2) (C 6 2) 1 []))]) Map.empty
      -- Adding a commercial zone
      let newZone = ZC (Rectangle (C 11 1) 2 2) []
      -- Adding another route that connects to existing routes and does not cause any collisions
      let newRoute = Route (HSegment (C 11 0) 5) 
      -- Attempt to construct a new city with both the new commercial zone and an additional route
      let intermediateVille = construit initialVille newRoute
      let finalResult = maybe Nothing (`construit` newZone) intermediateVille
      finalResult `shouldSatisfy` isJust