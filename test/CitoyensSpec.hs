module CitoyensSpec (spec) where

import Citoyens
import Types
import Model
import Data.Map as Map
import Test.Hspec
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe)

spec :: Spec
spec = do
    describe "creerImmigrant" $ do
        it "ajoute un immigrant à la ville" $ do
            let villeInitiale = Ville Map.empty Map.empty
                (villeFinale, cid) = creerImmigrant (C 1 1) (100, 0, 0) (Deplacement (C 0 0)) villeInitiale
                citoyenAttendu = Immigrant (C 1 1) (100, 0, 0) (Deplacement (C 0 0))
            Map.lookup cid (viCit villeFinale) `shouldBe` Just citoyenAttendu

    describe "transformerImmigrantEnHabitant" $ do
        it "transforme un immigrant en habitant" $ do
            let villeInitiale = Ville Map.empty (Map.singleton (CitId "Imm1") (Immigrant (C 0 0) (100, 0, 0) (Deplacement (C 0 0))))
                villeFinale = transformerImmigrantEnHabitant (CitId "Imm1") (BatId "Home1") villeInitiale (C 3 3)
                citoyenAttendu = Habitant (C 0 0) (100, 0, 0) ((BatId "Home1"), Nothing, Nothing) (Deplacement (C 3 3))
            Map.lookup (CitId "Imm1") (viCit villeFinale) `shouldBe` Just citoyenAttendu

    describe "mettreAJourOccupation" $ do
        it "met à jour l'occupation d'un citoyen" $ do
            let villeInitiale = Ville Map.empty (Map.singleton (CitId "Imm1") (Immigrant (C 1 1) (100, 0, 0) (Deplacement (C 3 3))))
                villeFinale = mettreAJourOccupation villeInitiale (CitId "Imm1") Travail
                citoyenAttendu = Immigrant (C 1 1) (100, 0, 0) Travail
            Map.lookup (CitId "Imm1") (viCit villeFinale) `shouldBe` Just citoyenAttendu

    describe "deplacerCitoyen" $ do
        it "déplace un citoyen vers une nouvelle coordonnée" $ do
            let villeInitiale = Ville Map.empty (Map.singleton (CitId "Hab1") (Habitant (C 1 1) (100, 0, 0) ((BatId "Home1"), Nothing, Nothing) (Deplacement (C 3 3))))
                villeFinale = mettreAJourCoord villeInitiale (CitId "Hab1") (C 2 2)
                citoyenAttendu = Habitant (C 2 2) (100, 0, 0) ((BatId "Home1"), Nothing, Nothing) (Deplacement (C 3 3))
            Map.lookup (CitId "Hab1") (viCit villeFinale) `shouldBe` Just citoyenAttendu

    describe "astarPathfinding" $ do
        it "trouve le chemin le plus court dans une ville vide" $ do
            let zones = Map.fromList [(ZoneId 1, Vide (Rectangle (C 0 0) 10 10))]
                ville = Ville zones Map.empty
                start = C 0 0
                end = C 2 2
                expectedPath = Just [C 0 0, C 1 0, C 2 0, C 2 1, C 2 2]
            traceShow (astarPathfinding start end ville) $
                astarPathfinding start end ville `shouldBe` expectedPath

        it "trouve le chemin le plus court en évitant les zones d'eau" $ do
            let zones = Map.fromList [(ZoneId 1, Vide (Rectangle (C 0 0) 10 10)), (ZoneId 2, Eau (Rectangle (C 1 1) 2 2))]
                ville = Ville zones Map.empty
                start = C 0 0
                end = C 3 3
                expectedPath = Just [C 0 0, C 1 0, C 2 0, C 3 0, C 3 1, C 3 2, C 3 3]
            astarPathfinding start end ville `shouldBe` expectedPath

    describe "determineNextStep" $ do
        it "transforme un immigrant en habitant s'il y a des maisons disponibles et renvoyer le target coord vers son appart" $ do
            let zones = Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 3 3) [Cabane (BatId "bat1") (Rectangle (C 1 1) 1 1) (C 1 1) 10 []])]
                ville = Ville zones Map.empty
                immigrantCoord = C 0 0
                (villeAvecImmigrant, immigrantId) = creerImmigrant immigrantCoord defaultEtat (Deplacement (C 0 0)) ville
                (newCoord, updatedVille) = determineNextStep (Immigrant immigrantCoord defaultEtat (Deplacement (C 0 0))) immigrantId villeAvecImmigrant
                citoyen = fromMaybe (error "Citoyen non trouvé") (Map.lookup immigrantId (viCit updatedVille))
          
            -- Vérifie que la prochaine étape est la coordonnée de la maison
            newCoord `shouldBe` (C 1 1)
            -- Vérifie que l'immigrant a été transformé en habitant
            case citoyen of
                Habitant _ _ (homeId, _, _) _ -> homeId `shouldBe` (BatId "bat1")
                _ -> expectationFailure "L'immigrant n'a pas été transformé en habitant"

    describe "deplacerCitoyen'" $ do
        it "déplace un habitant vers sa destination finale et met à jour l'occupation" $ do
            let zones = Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 3 3) [Cabane (BatId "bat1") (Rectangle (C 1 1) 1 1) (C 1 1) 10 []])]
                ville = Ville zones Map.empty
                habitant = Habitant (C 0 0) defaultEtat (BatId "bat1", Nothing, Nothing) (Deplacement (C 1 1))
                villeAvecHabitant = ville { viCit = Map.fromList [(CitId "1", habitant)] }
                villeApresDeplacement = deplacerCitoyen' villeAvecHabitant (CitId "1", habitant)
                citoyenApresDeplacement = fromMaybe (error "Citoyen non trouvé") (Map.lookup (CitId "1") (viCit villeApresDeplacement))
            getCoordCitoyen citoyenApresDeplacement `shouldBe` C 1 0
            getOccupation citoyenApresDeplacement `shouldBe` (Deplacement (C 1 1))
    
        it "transforme un immigrant en habitant s'il y a des maisons disponibles" $ do
            let zones = Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 3 3) [Cabane (BatId "bat1") (Rectangle (C 1 1) 1 1) (C 1 1) 10 []])]
                ville = Ville zones Map.empty
                immigrant = Immigrant (C 0 0) defaultEtat (Deplacement (C 0 0))
                villeAvecImmigrant = ville { viCit = Map.fromList [(CitId "1", immigrant)] }
                villeApresDeplacement = deplacerCitoyen' villeAvecImmigrant (CitId "1", immigrant)
                citoyenApresDeplacement = fromMaybe (error "Citoyen non trouvé") (Map.lookup (CitId "1") (viCit villeApresDeplacement))
            getCoordCitoyen citoyenApresDeplacement `shouldBe` C 1 0
            getOccupation citoyenApresDeplacement `shouldBe` (Deplacement (C 1 1))
        
        it "gère correctement plusieurs citoyens sur le même chemin" $ do
            let zones = Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 3 3) [Cabane (BatId "bat1") (Rectangle (C 1 1) 1 1) (C 1 1) 10 []])]
                ville = Ville zones Map.empty
                habitant1 = Habitant (C 0 0) defaultEtat (BatId "bat1", Nothing, Nothing) (Deplacement (C 1 1))
                habitant2 = Habitant (C 1 0) defaultEtat (BatId "bat1", Nothing, Nothing) (Deplacement (C 1 1))
                villeAvecHabitants = ville { viCit = Map.fromList [(CitId "1", habitant1), (CitId "2", habitant2)] }
                villeApresDeplacement1 = deplacerCitoyen' villeAvecHabitants (CitId "1", habitant1)
                villeApresDeplacement2 = deplacerCitoyen' villeApresDeplacement1 (CitId "2", habitant2)
                citoyenApresDeplacement1 = fromMaybe (error "Citoyen 1 non trouvé") (Map.lookup (CitId "1") (viCit villeApresDeplacement2))
                citoyenApresDeplacement2 = fromMaybe (error "Citoyen 2 non trouvé") (Map.lookup (CitId "2") (viCit villeApresDeplacement2))
            getCoordCitoyen citoyenApresDeplacement1 `shouldBe` C 0 1
            getCoordCitoyen citoyenApresDeplacement2 `shouldBe` C 1 1
            getOccupation citoyenApresDeplacement1 `shouldBe` (Deplacement (C 1 1))
            getOccupation citoyenApresDeplacement2 `shouldBe` (Deplacement (C 1 1))

        it "gère correctement plusieurs citoyens sur le même chemin" $ do
            let zones = Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 0) 3 3) [Cabane (BatId "bat1") (Rectangle (C 1 1) 1 1) (C 1 1) 10 []])]
                ville = Ville zones Map.empty
                habitant1 = Habitant (C 0 0) defaultEtat (BatId "bat1", Nothing, Nothing) (Deplacement (C 1 1))
                habitant2 = Habitant (C 1 0) defaultEtat (BatId "bat1", Nothing, Nothing) (Deplacement (C 1 0))
                villeAvecHabitants = ville { viCit = Map.fromList [(CitId "1", habitant1), (CitId "2", habitant2)] }
                villeApresDeplacement1 = deplacerCitoyen' villeAvecHabitants (CitId "1", habitant1)
                villeApresDeplacement2 = deplacerCitoyen' villeApresDeplacement1 (CitId "2", habitant2)
                citoyenApresDeplacement1 = fromMaybe (error "Citoyen 1 non trouvé") (Map.lookup (CitId "1") (viCit villeApresDeplacement2))
                citoyenApresDeplacement2 = fromMaybe (error "Citoyen 2 non trouvé") (Map.lookup (CitId "2") (viCit villeApresDeplacement2))
            getCoordCitoyen citoyenApresDeplacement1 `shouldBe` C 0 1
            getCoordCitoyen citoyenApresDeplacement2 `shouldBe` C 1 0
            getOccupation citoyenApresDeplacement1 `shouldBe` (Deplacement (C 1 1))
            getOccupation citoyenApresDeplacement2 `shouldBe` (Deplacement (C 1 0))
    
    describe "collecterImpotsCitoyens" $ do
        it "should collect taxes and rent from citizens" $ do
            let initialVille = Ville (Map.fromList []) (Map.fromList [
                            (CitId "cit1", Habitant (C 0 0) (100, 0, 0) (BatId "home1", Nothing, Nothing) Dodo),
                            (CitId "cit2", Habitant (C 0 0) (200, 0, 0) (BatId "home2", Nothing, Nothing) Dodo)
                            ])
                joueur = Joueur { fonds = 0, tauxImpotsCitoyens = 0.1, loyerCitoyens = 50 }
                (villeApresImpots, joueurApresImpots) = collecterImpotsCitoyens initialVille joueur

            fonds joueurApresImpots `shouldBe` 30
            let cit1 = fromMaybe (error "Citoyen non trouvé") (Map.lookup (CitId "cit1") (viCit villeApresImpots))
                cit2 = fromMaybe (error "Citoyen non trouvé") (Map.lookup (CitId "cit2") (viCit villeApresImpots))
            getArgent cit1 `shouldBe` 40
            getArgent cit2 `shouldBe` 130

    describe "collecterImpotsEtLoyerCitoyen" $ do
        it "should correctly calculate taxes and rent for a single citizen" $ do
            let joueur = Joueur { fonds = 0, tauxImpotsCitoyens = 0.1, loyerCitoyens = 50 }
                citoyen = Habitant (C 0 0) (100, 0, 0) (BatId "home1", Nothing, Nothing) Dodo
                (nouveauCitoyen, impots) = calculerImpotsEtLoyerCitoyen (tauxImpotsCitoyens joueur) (loyerCitoyens joueur) citoyen

            impots `shouldBe` 10
            getArgent nouveauCitoyen `shouldBe` 40
