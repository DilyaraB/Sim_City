module Batiments where 

import qualified Data.Map as Map
import Data.Map ( (!))
import Data.Maybe (fromMaybe)
import Zones
import Types

getBatId :: Batiment -> BatId
getBatId (Cabane bid _ _ _ _) = bid
getBatId (Atelier bid _ _ _ _) = bid
getBatId (Epicerie bid _ _ _ _) = bid
getBatId (Commissariat bid _ _) = bid

--verifie si un batiment a une entree adjacente a une route
validBatiment :: Batiment -> [Forme] -> Bool
validBatiment (Cabane _ _ entree _ _)  = any (adjacent entree) 
validBatiment (Atelier _ _ entree _ _)  = any (adjacent entree) 
validBatiment (Epicerie _ _ entree _ _)  = any (adjacent entree) 
validBatiment (Commissariat _ _ entree)  = any (adjacent entree) 

-- les cabanes/ateliers/epiceries ne se trouvent que dans les zones r ́esidentielles/industrielles/commerciales
canPlaceInZone :: Batiment -> Zone -> Bool
canPlaceInZone (Cabane _ _ _ _ _) (ZR _ _) = True
canPlaceInZone (Atelier _ _ _ _ _) (ZI _ _) = True
canPlaceInZone (Epicerie _ _ _ _ _) (ZC _ _) = True
canPlaceInZone (Commissariat _ _ _) _ = True 
canPlaceInZone _ _ = False

routes :: Ville -> [Forme]
routes ville = fmap zoneForme (filter estRoute (Map.elems (viZones ville))) 

--invariant verifiant : validBatiment pour tous les batiments
invariant_batiments :: Ville -> Bool
invariant_batiments ville = all isBuildingValid (concatMap extractBuildingsAndZones (Map.toList (viZones ville)))
  where

    extractBuildingsAndZones :: (ZoneId, Zone) -> [(Batiment, Zone)]
    extractBuildingsAndZones (_, zone) = case zone of
      ZR _ bs -> zip bs (repeat zone)
      ZI _ bs -> zip bs (repeat zone)
      ZC _ bs -> zip bs (repeat zone)
      _ -> []

    -- Validate each building against its zone and route adjacency
    isBuildingValid :: (Batiment, Zone) -> Bool
    isBuildingValid (batiment, zone) = validBatiment batiment (routes ville) && canPlaceInZone batiment zone


--precondition pour ajouterBatiment
precondition_ajouterBatiment :: Ville -> ZoneId -> Batiment -> Bool
precondition_ajouterBatiment ville zoneId batiment =
  case Map.lookup zoneId (viZones ville) of
    Just zone -> validBatiment batiment (routes ville) && canPlaceInZone batiment zone
    Nothing -> False


--verifie l'ajout de batimant a reussi 
postcondition_ajouterBatiment :: Ville -> ZoneId -> Batiment -> Maybe Ville -> Bool
postcondition_ajouterBatiment _ _ _ Nothing = True
postcondition_ajouterBatiment ville zoneId batiment (Just nouvelleVille) =
  case Map.lookup zoneId (viZones nouvelleVille) of
    Just zone -> elem batiment (getBatiments zone) && length (getBatiments zone) == length (getBatiments (viZones ville ! zoneId)) + 1
    Nothing -> False


--ajoute un batiment a une zone
ajouterBatiment :: Zone -> Batiment -> Maybe Zone
ajouterBatiment zone batiment 
  | canPlaceInZone batiment zone = Just $ case zone of
      ZR forme bs -> ZR forme (batiment : bs)
      ZI forme bs -> ZI forme (batiment : bs)
      ZC forme bs -> ZC forme (batiment : bs)
      _ -> zone 
  | otherwise = Nothing

--ajoute un batiment a une ville
ajouterBatimentVille :: Ville -> ZoneId -> Batiment -> Maybe Ville
ajouterBatimentVille ville zoneId batiment = do
    zone <- Map.lookup zoneId (viZones ville)
    nouvelleZone <- ajouterBatiment zone batiment
    let updatedZones = Map.insert zoneId nouvelleZone (viZones ville)
    return ville { viZones = updatedZones }


-- détermine le type de batiment à créer en fonction du type de zone
-- Détermine le type de bâtiment à créer en fonction du type de zone
determinerTypeBatiment :: Maybe Zone -> (BatId -> Forme -> Coord -> [CitId] -> Batiment)
determinerTypeBatiment (Just (ZR _ _)) = \bid f c ids -> Cabane bid f c 3 ids  
determinerTypeBatiment (Just (ZI _ _)) = \bid f c ids -> Atelier bid f c 3 ids  
determinerTypeBatiment (Just (ZC _ _)) = \bid f c ids -> Epicerie bid f c 3 ids  
determinerTypeBatiment _ = \bid f c _ -> Commissariat bid f c 


creerEtAjouterBatiment :: Coord -> Ville -> Maybe Forme -> Maybe [CitId] -> Maybe Ville
creerEtAjouterBatiment coord ville mforme mcitids =
    let formeDefaut = Rectangle coord 1 1
        forme = fromMaybe formeDefaut mforme
        citIds = fromMaybe [] mcitids
        totalBatiments = length (concatMap getBatiments (Map.elems (viZones ville)))
        batId = BatId $ show (totalBatiments + 1)
        maybeZoneId = trouveZoneIdParCoord coord ville
    in case maybeZoneId of
        Just zoneId -> do
            let zoneType = Map.lookup zoneId (viZones ville)
                createBuilding = determinerTypeBatiment zoneType
                nouveauBatiment = createBuilding batId forme coord citIds
            ajouterBatimentVille ville zoneId nouveauBatiment
        Nothing -> Nothing 

hasBatiment :: BatId -> Zone -> Bool
hasBatiment batId (ZR _ batiments) = any (\b -> getBatId b == batId) batiments
hasBatiment batId (ZI _ batiments) = any (\b -> getBatId b == batId) batiments
hasBatiment batId (ZC _ batiments) = any (\b -> getBatId b == batId) batiments
hasBatiment batId (Admin _ batiment) = getBatId batiment == batId
hasBatiment _ _ = False