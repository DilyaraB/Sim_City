module BatimentsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map

import Types
import Batiments
import Formes
import Zones

spec :: Spec
spec = do
    describe "Building Validation" $ do
        it "validates that a building's entrance is adjacent to a route and not within its own Forme" $ do
            let route = HSegment (C 10 0) 5 
            let building = Cabane (BatId "bat1") (Rectangle (C 5 0) 4 4) (C 9 0) 5 []
            validBatiment building [route] `shouldBe` True

    describe "Adding Buildings" $ do
        it "adds a building to a residential zone correctly" $ do
            let originalZone = ZR (Rectangle (C 0 0) 10 10) []
            let building = Cabane (BatId "bat2") (Rectangle (C 1 1) 2 2) (C 3 4) 2 []
            let updatedZone = ajouterBatiment originalZone building
            case updatedZone of
                Just updatedZone -> do
                    length (getBatiments updatedZone) `shouldBe` 1
                Nothing -> error "Building addition failed unexpectedly"

        it "does not add a cabane to an industrial zone" $ do
            let initialVille = Ville (Map.fromList [(ZoneId 2, ZI (Rectangle (C 0 0) 20 20) [])]) Map.empty
            let cabane = Cabane (BatId "bat3") (Rectangle (C 1 1) 3 3) (C 4 4) 5 []
            let result = ajouterBatimentVille initialVille (ZoneId 2) cabane
            result `shouldBe` Nothing
    
        it "ensures the precondition and postcondition when adding a building" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 10 10) []), (ZoneId 2, Route (VSegment (C 11 0) 5)) ]) Map.empty
            let building = Cabane (BatId "bat4") (Rectangle (C 8 1) 2 2) (C 10 3) 2 []
            let zoneId = ZoneId 1
            let validPrecondition = precondition_ajouterBatiment ville zoneId building
            validPrecondition `shouldBe` True

            let maybeNewVille = ajouterBatimentVille ville zoneId building
            let validPostcondition = postcondition_ajouterBatiment ville zoneId building maybeNewVille
            validPostcondition `shouldBe` True

        it "ensures the precondition and postcondition when adding a building" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 10 10) [])]) Map.empty
            let building = Cabane (BatId "bat5") (Rectangle (C 1 1) 2 2) (C 3 4) 2 []
            let zoneId = ZoneId 1
            let validPrecondition = precondition_ajouterBatiment ville zoneId building
            validPrecondition `shouldBe` False


