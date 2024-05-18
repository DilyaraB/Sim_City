module Zones where 

import Data.Map (Map)

data Zone = Eau Forme
          | Route Forme
          | ZR Forme [Batiment]
          | ZI Forme [Batiment]
          | ZC Forme [Batiment]
          | Admin Forme Batiment
  deriving (Show, Eq)

newtype ZoneId = ZoneId Int
  deriving (Show, Eq, Ord)

newtype BatId = BatId Int
  deriving (Show, Eq, Ord)

newtype CitId = CitId String
  deriving (Show, Eq, Ord)

data Ville = Ville {
  viZones :: Map ZoneId Zone,
  viCit :: Map CitId Citoyen
} deriving (Show, Eq)

--renvoie la forme de la zone
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f

formeToCoords :: Forme -> [Coord]
formeToCoords (HSegment (C x y) l) = [C (x + i) y | i <- [0..l-1]]
formeToCoords (VSegment (C x y) l) = [C x (y + i) | i <- [0..l-1]]
formeToCoords (Rectangle (C x y) w h) = [C (x + i) (y + j) | i <- [0..w-1], j <- [0..h-1]]

-------------------------------------------------------------

adjacent :: Coord -> Forme -> Bool
adjacent (C x y) forme =
  any (\(C x' y') -> (abs (x - x') == 1 && y == y') || (abs (y - y') == 1 && x == x')) (formeToCoords forme)

--verifie l'adjacence entre deux formes
adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = any (`adjacent` f2) (formeToCoords f1)

estRoute :: Zone -> Bool
estRoute (Route _) = True
estRoute _ = False

--recherche en profondeur pour routes_connexes
--parcourt les zones adjacentes 
dfs :: Ville -> ZoneId -> Set ZoneId -> Set ZoneId
dfs ville zid visited =
    --accumule les zones visitees
    foldl (\acc neighbor -> if member neighbor acc
                          then acc
                          else dfs ville neighbor (insert neighbor acc))
        visited
        (adjacentZones zid)
  where
    --les ids de zones qui sont adjacentes a la zid
    adjacentZones z = [zid' | (zid', z') <- toList (viZones ville), adjacentes (zoneForme (viZones ville ! z)) (zoneForme z')]

-------------------------------------------------------------

--verfie si aucune zone n'entre en collision avec une autre zone
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision ville =
  let zones = elems (viZones ville)
  in all (\(z1, z2) -> not (collision_approx (zoneForme z1) (zoneForme z2))) [(z1, z2) | z1 <- zones, z2 <- zones, z1 /= z2]

--invariant verifiant : prop_ville_sansCollision
invariant_disjoint_zones :: Ville -> Bool
invariant_disjoint_zones = prop_ville_sansCollision

--verifie si chaque zone résidentielle, commerciale, industrielle ou administrative est adjacente à au moins une zone de route
adjacente_a_route :: Ville -> Zone -> Bool
adjacente_a_route ville (Route _) = True
adjacente_a_route ville (Eau _) = True
adjacente_a_route ville z = any (adjacentes (zoneForme z) . zoneForme) (filter estRoute (elems (viZones ville)))

--invariant verifiant : adjacente_a_route pour tous les zones
invariant_adjacente_a_route :: Ville -> Bool
invariant_adjacente_a_route ville = all (adjacente_a_route ville) (elems (viZones ville))

--verifie si tous les routes sont connexes
routes_connexes :: Ville -> Bool
routes_connexes ville =
  let routes = [zid | (zid, Route _) <- toList (viZones ville)]
  in case routes of
       [] -> True
       (firstRoute:_) -> let visited = dfs ville firstRoute (Set.singleton firstRoute)
                        in Set.fromList routes == visited

--invariant verifiant : routes_connexes
invariant_routes_connexes :: Ville -> Bool
invariant_routes_connexes = routes_connexes

--verifie tous les invariants de la ville
invariants_ville :: Ville -> Bool
invariants_ville ville =
  invariant_disjoint_zones ville &&
  invariant_adjacente_a_route ville &&
  invariant_routes_connexes ville

--fonction qui construit(maybe) une nouvelle ville en ajoutant une zone a l'ancienne ville 
construit :: Ville -> Zone -> Maybe Ville
construit ville zone = let 
    zoneId = ZoneId (length (elems (viZones ville)) + 1)
    nouvelleVille = ville { viZones = insert zoneId zone (viZones ville) } in 
        if invariant_disjoint_zones nouvelleVille && 
            invariant_adjacente_a_route nouvelleVille && 
            invariant_routes_connexes nouvelleVille 
        then Just nouvelleVille
        else Nothing

--precondition pour construit
precondition_construit :: Ville -> Zone -> Bool
precondition_construit ville zone =
  invariant_disjoint_zones ville &&
  invariant_adjacente_a_route ville &&
  invariant_routes_connexes ville &&
  not (any (collision_approx (zoneForme zone)) (map zoneForme (elems (viZones ville))))

--postcondition pour construit
--verifie si zone est bien ajouté et si cette nouvelle ville verifie tous les invariants
postcondition_construit :: Ville -> Zone -> Maybe Ville -> Bool
postcondition_construit _ _ Nothing = True
postcondition_construit ville zone (Just nouvelleVille) =
  invariants_ville nouvelleVille &&
  (length (elems (viZones nouvelleVille)) == length (elems (viZones ville)) + 1)


