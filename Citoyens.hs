module Citoyens where

import Types
import Formes
import Zones
import Batiments

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', minimumBy)
import Data.Function (on)
import Debug.Trace

-- État par défaut pour les immigrants
defaultEtat :: (Int, Int, Int)
defaultEtat = (100, 0, 0)

defaultCoordImmigration :: Coord
defaultCoordImmigration = C 0 0

defaultCoordEmigration :: Coord
defaultCoordEmigration = C 0 0 ----a changer

citoyen2CitID::Citoyen->CitId
citoyen2CitID citizen = (CitId (show citizen))

newCitId :: Ville -> CitId
newCitId ville = CitId $ "Imm" ++ show (Map.size (viCit ville) + 1)

getCoordCitoyen :: Citoyen -> Coord
getCoordCitoyen (Habitant coord _ _ _) = coord
getCoordCitoyen (Immigrant coord _ _) = coord
getCoordCitoyen (Emigrant coord _) = coord

getOccupation :: Citoyen -> Occupation
getOccupation (Habitant _ _ _ occ) = occ
getOccupation (Immigrant _ _ occ) = occ
getOccupation (Emigrant _ occ) = occ

getArgent :: Citoyen -> Int
getArgent (Habitant _ (argent, _, _) _ _) = argent
getArgent (Immigrant _ (argent, _, _) _) = argent
getArgent (Emigrant _ _) = 0

deplacerCitoyen :: Citoyen->Coord->Citoyen
deplacerCitoyen 
	(Immigrant _ (cash,fatigue,faim) occ) coord 
	= (Immigrant coord (cash,fatigue,faim) occ)
deplacerCitoyen 
	(Habitant _ (cash,fatigue,faim) (maison, travail, course) occ) coord 
	= (Habitant coord (cash,fatigue,faim) (maison, travail, course) occ) 
deplacerCitoyen 
	(Emigrant _ occ) coord 
	= (Emigrant coord occ)

deplacerCitoyens :: Ville -> Ville
deplacerCitoyens ville =
    let citoyens = Map.toList (viCit ville)
    in foldl deplacerCitoyen' ville citoyens

deplacerCitoyen' :: Ville -> (CitId, Citoyen) -> Ville
deplacerCitoyen' ville (cid, citoyen) =
    case citoyen of
        Habitant coord etat (maison, travail, course) occ ->
            case occ of
                Deplacement targetCoord ->
                    let path = astarPathfinding (getCoordCitoyen citoyen) targetCoord ville
                    in case path of
                        Just [next] ->
                            let ville' 
                                  | isAtLocation next (Just maison) ville = mettreAJourOccupation ville cid Dodo
                                  | isAtLocation next travail ville = mettreAJourOccupation ville cid Travail
                                  | isAtLocation next course ville = mettreAJourOccupation ville cid Courses
                                  | otherwise = ville
                            in mettreAJourCoord ville' cid next
                        Just (_:next:_) -> mettreAJourCoord (mettreAJourOccupation ville cid (Deplacement targetCoord)) cid next
                        _ -> ville
                _ ->
                    let (nextCoord, ville') = determineNextStep citoyen cid ville
                        path = astarPathfinding (getCoordCitoyen citoyen) nextCoord ville'
                    in case path of
                        Just (_:next:_) -> mettreAJourCoord (mettreAJourOccupation ville' cid (Deplacement nextCoord)) cid next
                        _ -> ville'
        _ -> 
            let (nextCoord, ville') = determineNextStep citoyen cid ville
                path = astarPathfinding (getCoordCitoyen citoyen) nextCoord ville'
            in case path of
                Just (_:next:_) -> mettreAJourCoord (mettreAJourOccupation ville' cid (Deplacement nextCoord)) cid next
                _ -> ville'


-- Fonction pour ajouter un immigrant à la ville
creerDefaultImmigrant :: Ville -> Ville
creerDefaultImmigrant ville =
  let cid = newCitId ville
      immigrant = Immigrant defaultCoordImmigration defaultEtat (Deplacement (C 0 0))
      nouvelleMapCit = Map.insert cid immigrant (viCit ville)
  in ville { viCit = nouvelleMapCit }


creerImmigrant :: Coord -> (Int, Int, Int) -> Occupation -> Ville -> (Ville, CitId)
creerImmigrant coord etat occupation ville =
    let nouveauId = CitId $ "Imm" ++ show (Map.size (viCit ville) + 1) 
        nouveauImmigrant = Immigrant coord etat occupation
        nouvelleVille = ville { viCit = Map.insert nouveauId nouveauImmigrant (viCit ville) }
    in (nouvelleVille, nouveauId)

-- Preconditions
-- coord et l'etat valides
preconditionCreerImmigrant :: Coord -> (Int, Int, Int) -> Ville -> Bool
preconditionCreerImmigrant coord etat ville =
    coordValides coord ville && etatValide etat

-- Postconditions
-- verifie si l'ajout a reussi avec les attributs attendus
postconditionCreerImmigrant :: Coord -> (Int, Int, Int) -> Occupation -> Ville -> CitId -> Bool
postconditionCreerImmigrant coord etat occupation ville cid =
    case Map.lookup cid (viCit ville) of
        Just (Immigrant c e o) -> c == coord && e == etat && o == occupation
        _ -> False

transformerImmigrantEnHabitant :: CitId -> BatId -> Ville -> Coord -> Ville
transformerImmigrantEnHabitant cid homeId ville targetCoord =
    case Map.lookup cid (viCit ville) of
        Just (Immigrant coord etat _) ->
            let nouveauHabitant = Habitant coord etat (homeId, Nothing, Nothing) (Deplacement targetCoord)
            in ville { viCit = Map.insert cid nouveauHabitant (viCit ville) }
        _ -> ville

mettreAJourCoord :: Ville -> CitId -> Coord -> Ville
mettreAJourCoord ville cid newCoord =
    let citoyens = viCit ville
        citoyen = Map.lookup cid citoyens
        nouveauCitoyen = case citoyen of
                            Just (Habitant _ etat res occ) -> Habitant newCoord etat res occ
                            Just (Immigrant _ etat occ) -> Immigrant newCoord etat occ
                            Just (Emigrant _ occ) -> Emigrant newCoord occ
                            Nothing -> error "Citoyen non trouvé"
        nouveauxCitoyens = Map.insert cid nouveauCitoyen citoyens
    in ville { viCit = nouveauxCitoyens }

mettreAJourOccupation :: Ville -> CitId -> Occupation -> Ville
mettreAJourOccupation ville cid newOccupation =
    case Map.lookup cid (viCit ville) of
        Just citoyen ->
            let nouveauCitoyen = case citoyen of
                                                Habitant coord etat res _ -> Habitant coord etat res newOccupation
                                                Immigrant coord etat _ -> Immigrant coord etat newOccupation
                                                Emigrant coord _ -> Emigrant coord newOccupation
            in ville { viCit = Map.insert cid nouveauCitoyen (viCit ville) }
        _ -> ville

mettreAJourTravail :: Ville -> CitId -> BatId -> Ville
mettreAJourTravail ville cid newWorkId =
    case Map.lookup cid (viCit ville) of
        Just (Habitant coord etat (maison, _, course) occ) ->
            let nouveauCitoyen = Habitant coord etat (maison, Just newWorkId, course) occ
            in ville { viCit = Map.insert cid nouveauCitoyen (viCit ville) }
        _ -> ville

mettreAJourCourse :: Ville -> CitId -> BatId -> Ville
mettreAJourCourse ville cid newShopId =
    case Map.lookup cid (viCit ville) of
        Just (Habitant coord etat (maison, work, _) occ) ->
            let nouveauCitoyen = Habitant coord etat (maison, work, Just newShopId) occ
            in ville { viCit = Map.insert cid nouveauCitoyen (viCit ville) }
        _ -> ville


---------------- Invariants pour le citoyen ----------------------

--chaque citoyen doit etre dans une position valide
--les habitants doivent avoir une reference valide vers un batiment residentiel
invariantCitoyen :: Ville -> Citoyen -> Bool
invariantCitoyen ville citoyen = case citoyen of
    Immigrant coord etat _ -> coordValides coord ville && etatValide etat
    Habitant coord etat (homeId, workId, shopId) _ ->
        coordValides coord ville &&
        etatValide etat &&
        batimentExiste homeId ville &&
        maybe True (`batimentExiste` ville) workId &&
        maybe True (`batimentExiste` ville) shopId
    Emigrant coord _ -> coordValides coord ville


coordValides :: Coord -> Ville -> Bool
coordValides coord ville = any (coordInValidZone coord) (Map.elems (viZones ville))

coordInValidZone :: Coord -> Zone -> Bool
coordInValidZone (C x y) zone = case zone of
    Eau forme -> not $ appartient (C x y) forme
    Centrale forme -> not $ appartient (C x y) forme
    Cable forme -> not $ appartient (C x y) forme
    Admin forme _ -> not $ appartient (C x y) forme
    _ -> appartient (C x y) (zoneForme zone)

etatValide :: (Int, Int, Int) -> Bool
etatValide (argent, fatigue, faim) = fatigue >= 0 && fatigue <= 100 && faim >= 0 && faim <= 100


-- Vérifie si un bâtiment existe dans la ville et correspond au type attendu
batimentExiste :: BatId -> Ville -> Bool
batimentExiste batId ville = any (hasBatiment batId) (Map.elems (viZones ville))


--------for next step of citizen---------- 
determineNextStep :: Citoyen -> CitId -> Ville -> (Coord, Ville)
determineNextStep citoyen cid ville =
    case citoyen of
        Habitant coord etat (maison, travail, course) occ ->
            case occ of
                Travail
                    | shouldRest etat -> (getCoordById maison ville, ville)
                    | shouldEat etat -> case course of
                        Just storeId -> (getCoordById storeId ville, ville)
                        Nothing ->
                            case getAvailableStore coord ville of
                                Just (storeId, storeCoord) ->
                                    let ville' = mettreAJourCourse ville cid storeId
                                    in (storeCoord, ville')
                                Nothing -> (coord, ville)
                    | otherwise -> (coord, ville) --continue to work
                Dodo 
                    | shouldEat etat -> case course of
                        Just storeId -> (getCoordById storeId ville, ville)
                        Nothing ->
                            case getAvailableStore coord ville of
                                Just (storeId, storeCoord) ->
                                    let ville' = mettreAJourCourse ville cid storeId
                                    in (storeCoord, ville')
                                Nothing -> (coord, ville)
                    | shouldWork etat -> case travail of
                        Just workId -> (getCoordById workId ville, ville)
                        Nothing ->
                            case getAvailableJob coord ville of
                                Just (workId, workCoord) -> 
                                    let ville' = mettreAJourTravail ville cid workId
                                    in (workCoord, ville')
                                Nothing -> (coord, ville)
                    | otherwise -> (coord, ville) --continue to rest
                Courses
                    | shouldWork etat -> case travail of
                        Just workId -> (getCoordById workId ville, ville)
                        Nothing ->
                            case getAvailableJob coord ville of
                                Just (workId, workCoord) -> 
                                    let ville' = mettreAJourTravail ville cid workId
                                    in (workCoord, ville')
                                Nothing -> (coord, ville)
                    | otherwise -> (getCoordById maison ville, ville) -- go to home after courses       
        Immigrant coord _ _
            | checkAvailableHousing ville -> 
                 case getAvailableHouse coord ville of
                    Just (homeId, homeCoord) ->
                        let ville' = transformerImmigrantEnHabitant cid homeId ville homeCoord
                        in (homeCoord, ville')
            | otherwise -> (coord, ville)
        Emigrant _ _ -> (defaultCoordEmigration, ville)

shouldRest :: (Int, Int, Int) -> Bool
shouldRest (_, fatigue, faim) = fatigue > 50

shouldWork :: (Int, Int, Int) -> Bool
shouldWork (argent, _, _) = argent < 50

shouldEat :: (Int, Int, Int) -> Bool
shouldEat (_, _, faim) = faim > 50

shouldBeEmigrant :: Citoyen -> Bool
shouldBeEmigrant (Habitant _ (argent, fatigue, faim) _ _) = fatigue > 90 || argent < 5
shouldBeEmigrant (Immigrant _ (argent, fatigue, faim) _) = fatigue > 90 || argent < 5
shouldBeEmigrant _ = False

checkAvailableHousing :: Ville -> Bool
checkAvailableHousing ville =
    any isHousingAvailable (Map.elems (viZones ville))

isHousingAvailable :: Zone -> Bool
isHousingAvailable zone = case zone of
    ZR _ batiments -> any hasSpace batiments
    _ -> False

hasSpace :: Batiment -> Bool
hasSpace (Cabane _ _ _ maxOccupants occupants) = length occupants < maxOccupants
hasSpace (Atelier _ _ _ maxOccupants occupants) = length occupants < maxOccupants
hasSpace (Epicerie _ _ _ maxOccupants occupants) = length occupants < maxOccupants
hasSpace _ = False

checkAvailableJobs :: Ville -> Bool
checkAvailableJobs ville =
    any isJobAvailable (Map.elems (viZones ville))

isJobAvailable :: Zone -> Bool
isJobAvailable zone = case zone of
    ZI _ batiments -> any hasSpace batiments
    _ -> False

getBatEntree :: Batiment ->(BatId, Coord)
getBatEntree (Cabane id _ coord _ _) = (id, coord)
getBatEntree (Atelier id _ coord _ _) = (id, coord)
getBatEntree (Epicerie id _ coord _ _) = (id, coord)
getBatEntree (Commissariat id _ coord) = (id, coord)

getCoordById :: BatId -> Ville -> Coord
getCoordById batId ville =
    let zones = Map.elems (viZones ville)
        batiments = concatMap getBatiments zones
        coords = [(bId, coord) | (bId, coord) <- map getBatEntree batiments, bId == batId]
    in snd (head coords)

isAtLocation :: Coord -> Maybe BatId -> Ville -> Bool
isAtLocation coord (Just batId) ville =
    let batCoord = getCoordById batId ville
    in coord == batCoord
isAtLocation _ Nothing _ = False


getAvailableStore :: Coord -> Ville -> Maybe (BatId, Coord)
getAvailableStore coord ville = 
    let zones = Map.elems (viZones ville)
        stores = concatMap getStores zones
    in nearestBatiment coord stores

getStores :: Zone -> [Batiment]
getStores (ZC _ batiments) = filter hasSpace batiments
getStores _ = []

getAvailableHouse :: Coord -> Ville -> Maybe (BatId, Coord)
getAvailableHouse coord ville = 
    let zones = Map.elems (viZones ville)
        houses = concatMap getHouses zones
    in nearestBatiment coord houses

getHouses :: Zone -> [Batiment]
getHouses (ZR _ batiments) = filter hasSpace batiments
getHouses _ = []

getAvailableJob :: Coord -> Ville -> Maybe (BatId, Coord)
getAvailableJob coord ville = 
    let zones = Map.elems (viZones ville)
        houses = concatMap getJobs zones
    in nearestBatiment coord houses

getJobs :: Zone -> [Batiment]
getJobs (ZI _ batiments) = filter hasSpace batiments
getJobs _ = []

---nearest batiment(shop/work/home) to citoyen
nearestBatiment :: Coord -> [Batiment] -> Maybe (BatId, Coord)
nearestBatiment coord batiments
  | null batiments = Nothing
  | otherwise = Just $ minimumBy (compare `on` (heuristic coord . snd)) (map getBatEntree batiments)


---------------gerer les etats des citoyens---------------

augmenterArgent :: Int -> Citoyen -> Citoyen
augmenterArgent montant (Habitant coord (argent, fatigue, faim) res occ) =
    Habitant coord (argent + montant, fatigue, faim) res occ
augmenterArgent montant (Immigrant coord (argent, fatigue, faim) occ) =
    Immigrant coord (argent + montant, fatigue, faim) occ
augmenterArgent montant (Emigrant coord occ) = Emigrant coord occ

reduireArgent :: Int -> Citoyen -> Citoyen
reduireArgent montant = augmenterArgent (negate montant)

augmenterFatigue :: Int -> Citoyen -> Citoyen
augmenterFatigue montant (Habitant coord (argent, fatigue, faim) res occ) =
    Habitant coord (argent, max 100 (fatigue + montant), faim) res occ
augmenterFatigue montant (Immigrant coord (argent, fatigue, faim) occ) =
    Immigrant coord (argent, max 100 (fatigue + montant), faim) occ
augmenterFatigue montant (Emigrant coord occ) = Emigrant coord occ

reduireFatigue :: Int -> Citoyen -> Citoyen
reduireFatigue montant (Habitant coord (argent, fatigue, faim) res occ) =
    Habitant coord (argent, max 0 (fatigue - montant) , faim ) res occ
reduireFatigue montant (Immigrant coord (argent, fatigue, faim) occ) =
    Immigrant coord  (argent, max 0 (fatigue - montant) , faim ) occ
reduireFatigue montant (Emigrant coord occ) = Emigrant coord occ

augmenterFaim :: Int -> Citoyen -> Citoyen
augmenterFaim montant (Habitant coord (argent, fatigue, faim) res occ) =
    Habitant coord (argent, fatigue, max 100 (faim + montant)) res occ
augmenterFaim montant (Immigrant coord (argent, fatigue, faim) occ) =
    Immigrant coord (argent, fatigue, max 100 (faim + montant)) occ
augmenterFaim montant (Emigrant coord occ) = Emigrant coord occ

reduireFaim :: Int -> Citoyen -> Citoyen
reduireFaim montant (Habitant coord (argent, fatigue, faim) res occ) =
    Habitant coord (argent, fatigue, max 0 (faim - montant)) res occ
reduireFaim montant (Immigrant coord (argent, fatigue, faim) occ) =
    Immigrant coord (argent, fatigue, max 0 (faim - montant)) occ
reduireFaim montant (Emigrant coord occ) = Emigrant coord occ

miseAJourEtatCitoyen :: Citoyen -> Citoyen
miseAJourEtatCitoyen citoyen@(Habitant _ _ _ occ) = case occ of
    Travail -> augmenterArgent 10 . augmenterFatigue 5 $ citoyen
    Courses -> reduireArgent 5 . reduireFaim 10 $ citoyen
    Dodo -> reduireFatigue 10 $ citoyen
    Deplacement _ -> augmenterFatigue 3 $ citoyen
miseAJourEtatCitoyen citoyen@(Immigrant _ _ occ) = case occ of
    Travail -> augmenterArgent 10 . augmenterFatigue 5 $ citoyen
    Courses -> reduireArgent 5 . reduireFaim 10 $ citoyen
    Dodo -> reduireFatigue 10 $ citoyen
    Deplacement _ -> augmenterFatigue 3 $ citoyen
miseAJourEtatCitoyen citoyen@(Emigrant coord occ) = case occ of
    Deplacement _ -> augmenterFatigue 3 $ citoyen
    _ -> citoyen 

miseAJourEtatsCitoyens :: Ville -> Ville
miseAJourEtatsCitoyens ville = ville { viCit = Map.map miseAJourEtatCitoyen (viCit ville) }

augmenterFaimCitoyens :: Ville -> Ville
augmenterFaimCitoyens ville = ville { viCit = Map.map (augmenterFaim 1) (viCit ville) }

--------------- Pathfinding a* ----------------
astarPathfinding :: Coord -> Coord -> Ville -> Maybe [Coord]
astarPathfinding start end ville = aStar (Set.singleton (Node start 0 (heuristic start end))) Set.empty Map.empty where
    aStar reste visited cameFrom
        | Set.null reste = trace "Open set is empty, no path found" Nothing
        | coord currentNode == end = (Just (buildPath cameFrom end))
        | otherwise =
            let
                reste' = Set.delete currentNode reste
                visited' = Set.insert (coord currentNode) visited
                neighbors = filter (`Set.notMember` visited') $ getValidNeighbors (coord currentNode) ville
                newReste = foldr (processNeighbor currentNode) reste' neighbors
                cameFrom' = foldr (\neighbor cf -> Map.insert neighbor (coord currentNode) cf) cameFrom neighbors
            in aStar newReste visited' cameFrom'
        where
            currentNode = minimum reste
            processNeighbor currentNode neighbor reste =
                let newCost = costFromStart currentNode + 1 -- Coût pour se déplacer vers le voisin
                    newNode = Node neighbor newCost (newCost + heuristic neighbor end)
                in if Set.notMember neighbor (Set.map coord reste) || newCost < costFromStart (Set.elemAt 0 (Set.filter ((== neighbor) . coord) reste))
                   then Set.insert newNode reste
                   else reste

buildPath :: Map.Map Coord Coord -> Coord -> [Coord]
buildPath cameFrom current = buildPath' cameFrom current []

buildPath' :: Map.Map Coord Coord -> Coord -> [Coord] -> [Coord]
buildPath' cameFrom current path =
    case Map.lookup current cameFrom of
        Just prev -> buildPath' cameFrom prev (current : path)
        Nothing -> current : path

getValidNeighbors :: Coord -> Ville -> [Coord]
getValidNeighbors (C x y) ville = filter (isValidCoord ville) [C (x + 1) y, C (x - 1) y, C x (y + 1), C x (y - 1)]


isValidCoord :: Ville -> Coord -> Bool
isValidCoord ville coord = case trouveZoneIdParCoord coord ville of
        Just zoneId -> case Map.lookup zoneId (viZones ville) of
            Just zone -> isZoneTraversable zone && not (citoyenPresent ville coord)
            Nothing -> trace ("No zone found for coord: " ++ show coord) False
        Nothing -> trace ("No zoneId found for coord: " ++ show coord) False

isZoneTraversable :: Zone -> Bool
isZoneTraversable (Eau _) = False
isZoneTraversable (Centrale _) = False
isZoneTraversable (Centrale _) = False
isZoneTraversable (Admin _ _) = False
isZoneTraversable _ = True

citoyenPresent :: Ville -> Coord -> Bool 
citoyenPresent ville coord = any (\citoyen -> getCoordCitoyen citoyen == coord) (Map.elems (viCit ville))

heuristic :: Coord -> Coord -> Cost
heuristic (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

--------------gerer les immigrations/emigrations------------
gestionMigrations :: Ville -> Ville
gestionMigrations ville =
    let newVille = gererImmigration ville
    in gererEmigration newVille

gererImmigration :: Ville -> Ville
gererImmigration ville =
    if conditionsImmigration ville
    then creerDefaultImmigrant ville
    else ville

gererEmigration :: Ville -> Ville
gererEmigration ville =
    let villeAvecEmigrants = foldr maybeEmigrer ville (Map.toList (viCit ville))
    in foldr supprimerEmigrant villeAvecEmigrants (Map.toList (viCit ville))

-- Fonction pour transformer les citoyens en émigrants s'ils remplissent les conditions
maybeEmigrer :: (CitId, Citoyen) -> Ville -> Ville
maybeEmigrer (cid, citoyen) ville =
    if shouldBeEmigrant citoyen
    then
        let newEmigrant = case citoyen of
                              Habitant coord etat _ _ -> Emigrant coord (Deplacement defaultCoordEmigration)
                              Immigrant coord etat _ -> Emigrant coord (Deplacement defaultCoordEmigration)
                              _ -> citoyen
            newCitMap = Map.insert cid newEmigrant (viCit ville)
        in ville { viCit = newCitMap }
    else ville

-- Fonction pour supprimer les émigrants s'ils sont à la position d'émigration
supprimerEmigrant :: (CitId, Citoyen) -> Ville -> Ville
supprimerEmigrant (cid, Emigrant coord _) ville =
    if coord == defaultCoordEmigration
    then ville { viCit = Map.delete cid (viCit ville) }
    else ville
supprimerEmigrant _ ville = ville

conditionsImmigration :: Ville -> Bool
conditionsImmigration ville =
    let logementsDisponibles = checkAvailableHousing ville
        emploisDisponibles = checkAvailableJobs ville
    in logementsDisponibles && emploisDisponibles

------------Impots et Loyer---------------
---la fonction appellée chaque mois
---impots ce calcule selon son argent actuel

calculerImpotsEtLoyerCitoyen :: Float -> Int -> Citoyen -> (Citoyen, Int)
calculerImpotsEtLoyerCitoyen tauxImpot loyer (Habitant coord (argent, fatigue, faim) res occ) =
    let impots = floor (fromIntegral argent * tauxImpot)
        totalDeduction = impots + loyer
        nouvelArgent = max 0 (argent - totalDeduction)
    in (Habitant coord (nouvelArgent, fatigue, faim) res occ, impots)
calculerImpotsEtLoyerCitoyen _ _ citoyen = (citoyen, 0)