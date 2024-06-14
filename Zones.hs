module Zones where 

import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Control.Applicative
import Types
import Formes

--renvoie la forme de la zone
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Vide f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f
zoneForme (Centrale f) = f
zoneForme (Cable f) = f

formeToCoords :: Forme -> [Coord]
formeToCoords forme = 
  let coords = case forme of
        HSegment (C x y) l -> [C (x + i) y | i <- [0..l]]
        VSegment (C x y) l -> [C x (y + i) | i <- [0..l]]
        Rectangle (C x y) w h -> [C (x + i) (y + j) | i <- [0..w-1], j <- [0..h-1]]
  in coords


getBatiments :: Zone -> [Batiment]
getBatiments (ZR _ b) = b
getBatiments (ZI _ b) = b
getBatiments (ZC _ b) = b
getBatiments _ = []

notEmptyZone::Zone -> Bool
notEmptyZone z =
	case z of 
		Vide _ -> False 
		_ -> True

formeStartingPoint :: Forme -> Coord
formeStartingPoint (HSegment (C x y) _) = (C x y)
formeStartingPoint (VSegment (C x y) _) = (C x y )
formeStartingPoint (Rectangle (C x y) _ _) = (C x y)

filterEmptyZones :: Map.Map ZoneId Zone -> Map.Map ZoneId Zone
filterEmptyZones zmap = Map.filter (notEmptyZone) zmap

--verifie si une coord est dans la zone
coordInZone :: Coord -> Zone -> Bool
coordInZone coord zone = coord `elem` formeToCoords (zoneForme zone)

--trouve le zoneId correspondant a des coord donnes 
trouveZoneIdParCoord :: Coord -> Ville -> Maybe ZoneId
trouveZoneIdParCoord coord ville = 
    let zoneList = Map.toList (viZones ville)
        -- On cherche d'abord dans les zones non-vides
        nonEmptyZones = filter (not . isEmptyVide . snd) zoneList
        -- Si aucune zone non-vide ne correspond, on cherche dans les zones vides
        findZone zones = foldr (\(zoneId, zone) acc -> if coordInZone coord zone then Just zoneId else acc) Nothing zones
    in findZone nonEmptyZones <|> findZone (filter (isEmptyVide . snd) zoneList)

isEmptyVide :: Zone -> Bool
isEmptyVide (Vide _) = True
isEmptyVide _ = False

-- --- meme chose mais a partir du game state
-- trouveZoneIdParCoord' :: Coord -> GameState -> Maybe ZoneId
-- trouveZoneIdParCoord' coord gs = 
--   let cmap = getCMap gs in
--   Map.lookup coord cmap

adjacent :: Coord -> Forme -> Bool
adjacent (C x y) forme = any (\(C x' y') -> (abs (x - x') == 1 && y == y') || (abs (y - y') == 1 && x == x')) (formeToCoords forme)

--verifie l'adjacence entre deux formes
adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = any (`adjacent` f2) (formeToCoords f1)

estRoute :: Zone -> Bool
estRoute (Route _) = True
estRoute _ = False

--recherche en profondeur pour routes_connexes
--parcourt les zones adjacentes 
dfs :: Ville -> ZoneId -> Set.Set ZoneId -> Set.Set ZoneId
dfs ville zid visited =
    --accumule les zones visitees
    foldl (\acc neighbor -> if Set.member neighbor acc
                          then acc
                          else dfs ville neighbor (Set.insert neighbor acc))
        visited
        (adjacentRouteZones zid)
  where
    --les ids de zones qui sont adjacentes a la zid
    adjacentRouteZones z = [zid' | (zid', z') <- Map.toList (viZones ville), estRoute z' , adjacentes (zoneForme (viZones ville Map.! z)) (zoneForme z')]

--verfie si aucune zone n'entre en collision avec une autre zone
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision ville =
  let zones = Map.elems (filterEmptyZones (viZones ville))
  in all (\(z1, z2) -> not (collision_approx (zoneForme z1) (zoneForme z2))) [(z1, z2) | z1 <- zones, z2 <- zones, z1 /= z2]

--invariant verifiant : prop_ville_sansCollision
invariant_disjoint_zones :: Ville -> Bool
invariant_disjoint_zones = prop_ville_sansCollision

--verifie si chaque zone résidentielle, commerciale, industrielle ou administrative est adjacente à au moins une zone de route
adjacente_a_route :: Ville -> Zone -> Bool
adjacente_a_route ville (Route _) = True
adjacente_a_route ville (Eau _) = True
adjacente_a_route ville (Vide _) = True
adjacente_a_route ville z =
    let routes = filter estRoute (Map.elems (viZones ville))
    in if null routes
       then True -- S'il n'y a pas de routes, autoriser l'ajout de zones
       else any (adjacentes (zoneForme z) . zoneForme) routes

--invariant verifiant : adjacente_a_route pour tous les zones
invariant_adjacente_a_route :: Ville -> Bool
invariant_adjacente_a_route ville = all (adjacente_a_route ville) (Map.elems (filterEmptyZones (viZones ville)))

--verifie si tous les routes sont connexes
routes_connexes :: Ville -> Bool
routes_connexes ville =
  let routes = [zid | (zid, Route _) <- Map.toList (viZones ville)]
  in case routes of
       [] -> True
       [x]-> True
       (firstRoute:_) -> let visited = dfs ville firstRoute (Set.singleton firstRoute)
                        in traceShow ("Connected component check:", routes, visited) (Set.fromList routes == visited)

--invariant verifiant : routes_connexes
invariant_routes_connexes :: Ville -> Bool
invariant_routes_connexes = routes_connexes

--verifie tous les invariants de la ville

invariants_ville :: Ville -> Bool
invariants_ville ville =
  let disjointZones = invariant_disjoint_zones ville
      adjacenteRoute = invariant_adjacente_a_route ville
      routesConnexes = invariant_routes_connexes ville
  in trace ("invariant_disjoint_zones: " ++ show disjointZones) $
     trace ("invariant_adjacente_a_route: " ++ show adjacenteRoute) $
     trace ("invariant_routes_connexes: " ++ show routesConnexes) $
     disjointZones && adjacenteRoute && routesConnexes

--fonction qui construit(maybe) une nouvelle ville en ajoutant une zone a l'ancienne ville 
construit :: Ville -> Zone -> Maybe Ville
construit ville zone = let 
    zoneId = ZoneId (length (Map.elems (viZones ville)) + 1)
    nouvelleVille = ville { viZones = Map.insert zoneId zone (viZones ville) }
    logNouvelleVille = traceShow (nouvelleVille) nouvelleVille
    in if 
        traceShow "Checking if new city has disjoint zones:" (invariant_disjoint_zones logNouvelleVille) &&
        traceShow "Checking if every zone is adjacent to a route:" (invariant_adjacente_a_route logNouvelleVille) &&
        traceShow "Checking if all routes are connected:" (invariant_routes_connexes logNouvelleVille)
    then Just logNouvelleVille
    else trace "Failed to construct new city due to invariant failure." Nothing


--precondition pour construit
precondition_construit :: Ville -> Zone -> Bool
precondition_construit ville zone =
  invariant_disjoint_zones ville &&
  invariant_adjacente_a_route ville &&
  invariant_routes_connexes ville &&
  not (any (collision_approx (zoneForme zone)) (map zoneForme (Map.elems (viZones ville))))

--postcondition pour construit
--verifie si zone est bien ajouté et si cette nouvelle ville verifie tous les invariants
postcondition_construit :: Ville -> Zone -> Maybe Ville -> Bool
postcondition_construit _ _ Nothing = True
postcondition_construit ville zone (Just nouvelleVille) =
  invariants_ville nouvelleVille &&
  (length (Map.elems (viZones nouvelleVille)) == length (Map.elems (viZones ville)) + 1)


