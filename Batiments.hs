module Batiments where 

import Zones as Z
import Data.Map (lookup, (!))

data Batiment = Cabane Forme Coord Int [CitId]
              | Atelier Forme Coord Int [CitId]
              | Epicerie Forme Coord Int [CitId]
              | Commissariat Forme Coord
  deriving (Show, Eq)


--verifie si un batiment a une entree adjacente a une route
validBatiment :: Batiment -> [Forme] -> Bool
validBatiment (Cabane _ entree _ _) routes = any (`adjacent` entree) routes
validBatiment (Atelier _ entree _ _) routes = any (`adjacent` entree) routes
validBatiment (Epicerie _ entree _ _) routes = any (`adjacent` entree) routes
validBatiment (Commissariat _ entree) routes = any (`adjacent` entree) routes


--invariant verifiant : validBatiment pour tous les batiments
invariant_batiments :: Ville -> Bool
invariant_batiments ville = all (\b -> validBatiment b routes) (concatMap batiments (elems (viZones ville)))
  where
    routes = concatMap (Z.formeToCoords . zoneForme) (filter estRoute (elems (viZones ville)))
    
    batiments (ZR _ bs) = bs
    batiments (ZI _ bs) = bs
    batiments (ZC _ bs) = bs
    batiments _         = []


--ajoute un batiment a une zone
ajouterBatiment :: Zone -> Batiment -> Zone
ajouterBatiment (ZR forme batiments) batiment = ZR forme (batiment : batiments)
ajouterBatiment (ZI forme batiments) batiment = ZI forme (batiment : batiments)
ajouterBatiment (ZC forme batiments) batiment = ZC forme (batiment : batiments)
ajouterBatiment zone _ = zone

--precondition pour ajouterBatiment
precondition_ajouterBatiment :: Ville -> ZoneId -> Batiment -> Bool
precondition_ajouterBatiment ville zoneId batiment =
  case lookup zoneId (viZones ville) of
    Just (ZR _ _) -> validBatiment batiment routes
    Just (ZI _ _) -> validBatiment batiment routes
    Just (ZC _ _) -> validBatiment batiment routes
    _ -> False
  where
    routes = concatMap (formeToCoords . zoneForme) (filter estRoute (elems (viZones ville)))

--verifie l'ajout de batimant a reussi 
postcondition_ajouterBatiment :: Ville -> ZoneId -> Batiment -> Maybe Ville -> Bool
postcondition_ajouterBatiment _ _ _ Nothing = True
postcondition_ajouterBatiment ville zoneId batiment (Just nouvelleVille) =
  case lookup zoneId (viZones nouvelleVille) of
    Just zone -> elem batiment (batiments zone) && length (batiments zone) == length (batiments (viZones ville ! zoneId)) + 1
    Nothing -> False
  where
    batiments (ZR _ bs) = bs
    batiments (ZI _ bs) = bs
    batiments (ZC _ bs) = bs
    batiments _         = []

--ajoute un batiment a une ville
ajouterBatimentVille :: Ville -> ZoneId -> Batiment -> Maybe Ville
ajouterBatimentVille ville zoneId batiment =
  case lookup zoneId (viZones ville) of
    Just zone ->
      if precondition_ajouterBatiment ville zoneId batiment
      then let nouvelleZone = ajouterBatiment zone batiment
           in construit ville nouvelleZone
      else Nothing
    Nothing -> Nothing
