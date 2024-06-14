module Model where

import SDL
import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Formes
import Zones
import Citoyens
import qualified Data.Set as Set

import Input.Mouse (Mouse)
import qualified Input.Mouse as M

import Input.Keyboard (Keyboard)
import qualified Input.Keyboard as K
import Debug.Trace

data Joueur = Joueur { 
	fonds::Int,
	tauxImpotsCitoyens :: Float,
	loyerCitoyens :: Int
} deriving (Show)

data LineEnd = Unset 
			  | Ending Coord 
	deriving (Show,Eq)

data Lining = LineEnd LineEnd
	deriving (Show,Eq)

data GameMode =   Build
				| BuildZR
				| BuildZI
				| BuildZC 
				| BuildCentrale
				| BuildCable LineEnd LineEnd
				| View
				| Lining LineEnd LineEnd
	deriving (Show,Eq)

data GameState = GameState {  mode::GameMode
							, coordMap:: Map Coord ZoneId
							, gameMap::Ville
							, player::Joueur}
	deriving (Show)


displayGameMode::GameState->String
displayGameMode gs@(GameState{mode=gm}) =
	case gm of 
		View -> "View"
		Build -> "Flooding"
		Lining Unset Unset -> "Setting Road -- both unset"
		Lining (Ending (C x y)) Unset -> "Setting Road -- first: ("<>(show x)<>" , "<>(show y)<>")"
		BuildCentrale -> "Centrale"
		BuildCable Unset Unset -> "Setting Cable -- both unset"
		BuildCable (Ending (C x y)) Unset -> "Setting Cable -- first: ("<>(show x)<>" , "<>(show y)<>")"
		BuildZR -> "ZR"
		BuildZI -> "ZI"
		BuildZC -> "ZC"
		_ -> "Cheating"

initJoueur::Joueur
initJoueur = Joueur {
    fonds = 1000,
    tauxImpotsCitoyens = 0.1,
    loyerCitoyens = 50
} 
    -- tauxImpotsAteliers = 0.2,
    -- tauxImpotsEpiceries = 0.25
    --memes taux d'impots et loyer pour tous les citoyens pour l'instant

getJoueurFunds::GameState->Int
getJoueurFunds gs@(GameState{player=j@(Joueur{fonds=x})}) = x

-- cree une grille vide 10*10
auxInitVille::Map ZoneId Zone
auxInitVille=
	let f= (\(x,y)-> Types.Rectangle (C x y) 1 1) in
	let coordList=[(x,y)|x<-[0..30],y<-[0..30]] in 
	let toMap=zip [ZoneId i | i<-[0..(length (fmap f coordList))]] [(Vide x)|x<-fmap f coordList] in 
	let mapped=Map.fromList toMap in
	mapped

auxCoordMap::Map Coord ZoneId
auxCoordMap=
	let coordList=[(C x y)|x<-[0..30],y<-[0..30]] in
	let toMap=zip coordList [ZoneId i | i<-[0..(length (coordList))]] in 
	Map.fromList toMap

convert2water::Coord -> Map Coord ZoneId -> Ville -> Ville 
convert2water (C x y) coordMap vil@(Ville{viZones=v,viCit=vc}) =
	case Map.lookup (C x y) coordMap of
		Just zid -> case Map.lookup zid v of
			Just (Vide f) -> 
				let v' = Map.insert zid (Eau f) v in
				vil{viZones=v',viCit=vc} 
			otherwise -> vil
		otherwise -> vil

convert2water'::Coord -> GameState->GameState
convert2water' co gs@(GameState{coordMap=cm, gameMap=v}) =
	let v' = convert2water co cm v in 
	gs {coordMap=cm,gameMap=v'}


initVille::Ville
initVille = Ville (auxInitVille) (Map.empty)

initGameState::GameState
initGameState= GameState View (auxCoordMap) (initVille) (initJoueur)

---Accessors
getVilleMap_gs::GameState->Map ZoneId Zone
getVilleMap_gs gs=(getVilleMap (getVille gs))

getVilleMap::Ville->Map ZoneId Zone
getVilleMap vil@(Ville{viZones=v,viCit=_})=v

getVille::GameState->Ville
getVille gs@(GameState{gameMap=v})=v

getCMap::GameState->Map Coord ZoneId
getCMap gs@(GameState{coordMap=cm})=cm

switchMode::GameState -> GameState
switchMode gs@(GameState{mode=m,gameMap=map,player=p}) =
	case m of 
	 Build ->  gs {mode=View,gameMap=map,player=p}
	 _ -> gs {mode=Build,gameMap=map,player=p}

toggleLining::GameState -> GameState 
toggleLining gs@(GameState{mode=m,gameMap=map,player=p}) =
	case m of 
	 Lining _ _ ->  gs {mode=View,gameMap=map,player=p}
	 _ -> gs {mode=Lining Unset Unset,gameMap=map,player=p}

toggleBuildCable::GameState -> GameState 
toggleBuildCable gs@(GameState{mode=m,gameMap=map,player=p}) =
	case m of 
	 BuildCable _ _ ->  gs {mode=View,gameMap=map,player=p}
	 _ -> gs {mode=BuildCable Unset Unset,gameMap=map,player=p}

toggleBuildCentrale::GameState -> GameState 
toggleBuildCentrale gs@(GameState{mode=m}) =
	case m of 
	 BuildCentrale ->  gs {mode=View}
	 _ -> gs {mode=BuildCentrale}

toggleBuildZR::GameState -> GameState 
toggleBuildZR gs@(GameState{mode=m}) =
	case m of 
	 BuildZR ->  gs {mode=View}
	 _ -> gs {mode=BuildZR}

toggleBuildZI::GameState -> GameState 
toggleBuildZI gs@(GameState{mode=m}) =
	case m of 
	 BuildZI ->  gs {mode=View}
	 _ -> gs {mode=BuildZI}

toggleBuildZC::GameState -> GameState 
toggleBuildZC gs@(GameState{mode=m}) =
	case m of 
	 BuildZC ->  gs {mode=View}
	 _ -> gs {mode=BuildZC}

lookupByCoord::Coord->GameState->Maybe Zone
lookupByCoord co gs@(GameState{coordMap=cMap, gameMap=v}) =
	case Map.lookup co cMap of 
		Just zid -> case Map.lookup zid (getVilleMap v) of 
			Just z -> Just z
			Nothing -> Nothing
		Nothing -> Nothing

checkBuildMode::GameState -> Bool
checkBuildMode gs@(GameState{mode=m}) =
	case m of 
		Build -> True
		_ -> False

updateGameState_mode::GameState->GameMode->GameState
updateGameState_mode gs@(GameState{mode=m,coordMap=cmap,gameMap=vil,player=p}) new = 
	gs{mode=new}

updateGameState_ville::GameState->Ville->GameState
updateGameState_ville gs@(GameState{mode=m,coordMap=cmap,gameMap=vil,player=p}) new = 
	gs{gameMap=new}

updateGameState_coordMap::GameState->Map Coord ZoneId->GameState
updateGameState_coordMap gs@(GameState{mode=m,coordMap=cmap,gameMap=vil,player=p}) new = 
	gs{coordMap=new}

updateGameState_player::GameState->Joueur->GameState
updateGameState_player gs@(GameState{mode=m,coordMap=cmap,gameMap=vil,player=p}) new = 
	gs{player=new}

updateVille_viZ::Ville->Map ZoneId Zone -> Ville
updateVille_viZ vil@(Ville{viZones=viz,viCit=vic}) new =
	vil{viZones=new}

updateVille_viC::Ville->Map CitId Citoyen -> Ville
updateVille_viC vil@(Ville{viZones=viz,viCit=vic}) new =
	vil{viCit=new}

updateTauxImpotsCitoyens :: Joueur -> Float -> Joueur
updateTauxImpotsCitoyens joueur newTaux = joueur { tauxImpotsCitoyens = newTaux }

updateLoyerCitoyens :: Joueur -> Int -> Joueur
updateLoyerCitoyens joueur newLoyer = joueur { loyerCitoyens = newLoyer }

-----------------------------------
--collecte les impots/les loyer a partir des citoyens et 
--ajuste le fond du joueur et des citoyens

collecterImpotsCitoyens :: Ville -> Joueur -> (Ville, Joueur)
collecterImpotsCitoyens ville joueur =
    let citoyens = Map.toList (viCit ville)
        (ville', totalImpots) = foldl (collecterImpotsEtLoyerCitoyen joueur) (ville, 0) citoyens
        joueur' = joueur { fonds = fonds joueur + totalImpots }
    in (ville', joueur')

collecterImpotsEtLoyerCitoyen :: Joueur -> (Ville, Int) -> (CitId, Citoyen) -> (Ville, Int)
collecterImpotsEtLoyerCitoyen joueur (ville, totalImpots) (cid, citoyen) =
    let (nouveauCitoyen, impots) = calculerImpotsEtLoyerCitoyen (tauxImpotsCitoyens joueur) (loyerCitoyens joueur) citoyen
        ville' = ville { viCit = Map.insert cid nouveauCitoyen (viCit ville) }
    in (ville', totalImpots + impots)


------------------------------------

--- accompany with getRoadForme from user input, vertical/horizontal expanse (min 2 tiles)
--- get [forme] from 2 points, either straight line or manhattan distance ....

roadShapeGuesser::Coord->Coord->[Forme]
roadShapeGuesser (C x y) (C x' y') = 
	if (x==x' && y==y') then [] else
	if x==x' then
		if y>y' then [VSegment (C x' y') (y-y')] else [VSegment (C x' y) (y'-y)]
	else 
	if y==y' then
		if x>x' then [HSegment (C x' y) (x-x')] else [HSegment (C x y') (x'-x)]
	else
		let dx=abs (x-x') in let dy = abs (y-y') in
		if (x>x' && y>y') then [(HSegment (C x' y') dx),(VSegment (C (x'+dx) y') dy)]
		else if (x>x' && y<y') then [(HSegment (C x' y') dx),(VSegment (C x y) dy)]
		else if (x<x' && y<y') then [(HSegment (C x y) dx),(VSegment (C (x+dx) y) dy)]
		else if (x<x' && y>y') then [(HSegment (C x y) dx),(VSegment (C x' y') dy)]
		else []


insertRoad::Forme -> GameState -> GameState
insertRoad f gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let cmap' = foldr (\x acc -> Map.insert x newZoneId acc) cmap mk in
	let zmap' = Map.insert (newZoneId) (Route f) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		updatePlayerFunds (updateGameState_coordMap (updateGameState_ville gs v') cmap') (\x->x-10*(length $ formeToCoords f))


insertRoads::[Forme] -> GameState -> GameState
insertRoads fs gs =
	foldr (\x acc -> insertRoad x acc) gs fs

-------------------------------------------------------------

insertCable::Forme -> GameState -> GameState
insertCable f gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let zmap' = Map.insert (newZoneId) (Cable f) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		updatePlayerFunds (updateGameState_ville gs v') (\x->x-10*(length $ formeToCoords f))

insertCables::[Forme] -> GameState -> GameState
insertCables fs gs =
	foldr (\x acc -> insertCable x acc) gs fs


-------------------------------------------------------------

updatePlayerFunds::GameState->(Int->Int)->GameState
updatePlayerFunds gs@(GameState{player=j@(Joueur{fonds=x})}) f =
	gs{player=j{fonds=f x}}


insertZR::Coord->GameState->GameState 
insertZR co gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let f = (Types.Rectangle (co) 3 3) in 
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let cmap' = foldr (\x acc -> Map.insert x newZoneId acc) cmap mk in
	let zmap' = Map.insert (newZoneId) (ZR f []) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		if invariant_disjoint_zones v' 
			then updatePlayerFunds (updateGameState_coordMap (updateGameState_ville gs v') cmap') (\x->x-100)
		else gs

insertZI::Coord->GameState->GameState 
insertZI co gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let f = (Types.Rectangle (co) 3 3) in 
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let cmap' = foldr (\x acc -> Map.insert x newZoneId acc) cmap mk in
	let zmap' = Map.insert (newZoneId) (ZI f []) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		if invariant_disjoint_zones v' 
			then updatePlayerFunds (updateGameState_coordMap (updateGameState_ville gs v') cmap') (\x->x-300)
		else gs

insertZC::Coord->GameState->GameState 
insertZC co gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let f = (Types.Rectangle (co) 3 3) in 
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let cmap' = foldr (\x acc -> Map.insert x newZoneId acc) cmap mk in
	let zmap' = Map.insert (newZoneId) (ZC f []) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		if invariant_disjoint_zones v' 
			then updatePlayerFunds (updateGameState_coordMap (updateGameState_ville gs v') cmap') (\x->x-200)
		else gs


insertCentrale::Coord->GameState->GameState 
insertCentrale co gs =
	let cmap = getCMap gs in
	let zmap = getVilleMap (getVille gs) in 
	let newZoneId=ZoneId $ (length zmap)+1 in
	let f = (Types.Rectangle (co) 3 3) in 
	let affectedTiles = Map.filterWithKey (\k _ -> appartient' f k) cmap in
	let mk = Map.keys (Map.filterWithKey (\k _ -> appartient' f k) cmap) in
	let cmap' = foldr (\x acc -> Map.insert x newZoneId acc) cmap mk in
	let zmap' = Map.insert (newZoneId) (Centrale f) zmap in 
	let v' = updateVille_viZ (getVille gs) zmap' in
		if invariants_ville v' 
			then updatePlayerFunds (updateGameState_coordMap (updateGameState_ville gs v') cmap') (\x->x-500)
		else gs

--- ajouter safeInserts en verifiant les invariants & pre/post conditions

------------------------------------
-- Zone evolution

isOverlay::Zone->Bool
isOverlay z =
	case z of
		Eau _ -> False
		Vide _ -> False
		Route _ -> False
		_ -> True 

isZR::Zone->Bool
isZR z =
	case z of 
		ZR _ _ -> True
		_ -> False

isZI::Zone->Bool
isZI z =
	case z of 
		ZI _ _ -> True
		_ -> False

isZC::Zone->Bool
isZC z =
	case z of 
		ZC _ _ -> True
		_ -> False

isCentrale::Zone->Bool
isCentrale z =
	case z of 
		Centrale _  -> True
		_ -> False

filterOverlays::Map ZoneId Zone -> Map ZoneId Zone
filterOverlays zmap = Map.filter (isOverlay) zmap

filterZRs::Map ZoneId Zone -> Map ZoneId Zone
filterZRs zmap = Map.filter (isZR) zmap

filterZIs::Map ZoneId Zone -> Map ZoneId Zone
filterZIs zmap = Map.filter (isZI) zmap

filterZCs::Map ZoneId Zone -> Map ZoneId Zone
filterZCs zmap = Map.filter (isZC) zmap

filterCentrales::Map ZoneId Zone -> Map ZoneId Zone
filterCentrales zmap = Map.filter (isCentrale) zmap

filterEmptyZones_gs::GameState -> Map ZoneId Zone
filterEmptyZones_gs gs = 
	let zmap = (getVilleMap (getVille gs)) in
	Zones.filterEmptyZones zmap

filterOverlays_gs::GameState -> Map ZoneId Zone
filterOverlays_gs gs = 
	let zmap = (getVilleMap (getVille gs)) in
	filterOverlays zmap

filterZRs_gs::GameState-> Map ZoneId Zone
filterZRs_gs gs =
	let zmap = (getVilleMap (getVille gs)) in
	filterZRs zmap

filterCentrales_gs::GameState-> Map ZoneId Zone
filterCentrales_gs gs =
	let zmap = (getVilleMap (getVille gs)) in
	filterCentrales zmap

------------------------------------------

batimentStartingCoord :: Batiment -> Coord
batimentStartingCoord (Cabane _ f _ _ _)  = formeStartingPoint f
batimentStartingCoord (Atelier _ f _ _ _)  = formeStartingPoint f
batimentStartingCoord (Epicerie _ f _ _ _)  = formeStartingPoint f 
batimentStartingCoord (Commissariat _ f _)  = formeStartingPoint f 

ajouterBatimentsZR :: Zone -> Zone
ajouterBatimentsZR (ZR (Types.Rectangle (C x y) w h) batlist) =
	if length batlist == w*h then (ZR (Types.Rectangle (C x y) w h) batlist)
	else 
	let batPos = 
		if length batlist == 0 then (C x y)
		else 
			let candidatePos = [p | p<-formeToCoords (Types.Rectangle (C x y) w h), not (p `elem` (map batimentStartingCoord batlist))] 
			in
			head candidatePos
	in	
	let defaultBatiment = 
		(Cabane 
		(BatId ("Cab"<>(show (length batlist))))
		(Types.Rectangle batPos 1 1)
		batPos
		10
		[])
	in (ZR (Types.Rectangle (C x y) w h) (batlist<>[defaultBatiment]))


ajouterBatimentsZR_Ville :: Ville -> Ville
ajouterBatimentsZR_Ville ville =
	let vilmap = getVilleMap ville in 
	let zrs = filterZRs vilmap in 
	let filtered_twice = checkConditions zrs in
	let zip_zrs = zip (Map.keys filtered_twice) (Map.elems filtered_twice) in
	let withBuildings = map (\(k,v) -> (k,ajouterBatimentsZR v)) zip_zrs in
	let vilmap' = foldr (\(k,v) acc -> Map.insert k v acc) vilmap withBuildings in  
	let vil' = updateVille_viZ ville vilmap'
	in vil' 
	where
		checkConditions zones_res = Map.filter (adjacente_a_route ville) zones_res


ajouterBatimentsZI :: Zone -> Zone
ajouterBatimentsZI (ZI (Types.Rectangle (C x y) w h) batlist) =
	if length batlist == w*h then (ZI (Types.Rectangle (C x y) w h) batlist)
	else 
	let batPos = 
		if length batlist == 0 then (C x y)
		else 
			let candidatePos = [p | p<-formeToCoords (Types.Rectangle (C x y) w h), not (p `elem` (map batimentStartingCoord batlist))] 
			in
			head candidatePos
	in	
	let defaultBatiment = 
		(Atelier 
		(BatId ("Atelier"<>(show (length batlist))))
		(Types.Rectangle batPos 1 1)
		batPos
		10
		[])
	in (ZI (Types.Rectangle (C x y) w h) (batlist<>[defaultBatiment]))


ajouterBatimentsZI_Ville :: Ville -> Ville
ajouterBatimentsZI_Ville ville =
	let vilmap = getVilleMap ville in 
	let zrs = filterZIs vilmap in 
	let filtered_twice = checkConditions zrs in
	let zip_zrs = zip (Map.keys filtered_twice) (Map.elems filtered_twice) in
	let withBuildings = map (\(k,v) -> (k,ajouterBatimentsZI v)) zip_zrs in
	let vilmap' = foldr (\(k,v) acc -> Map.insert k v acc) vilmap withBuildings in  
	let vil' = updateVille_viZ ville vilmap'
	in vil' 
	where
		checkConditions zones_res = Map.filter (adjacente_a_route ville) zones_res


ajouterBatimentsZC :: Zone -> Zone
ajouterBatimentsZC (ZC (Types.Rectangle (C x y) w h) batlist) =
	if length batlist == w*h then (ZI (Types.Rectangle (C x y) w h) batlist)
	else 
	let batPos = 
		if length batlist == 0 then (C x y)
		else 
			let candidatePos = [p | p<-formeToCoords (Types.Rectangle (C x y) w h), not (p `elem` (map batimentStartingCoord batlist))] 
			in
			head candidatePos
	in	
	let defaultBatiment = 
		(Epicerie 
		(BatId ("Epicerie"<>(show (length batlist))))
		(Types.Rectangle batPos 1 1)
		batPos
		10
		[])
	in (ZC (Types.Rectangle (C x y) w h) (batlist<>[defaultBatiment]))


ajouterBatimentsZC_Ville :: Ville -> Ville
ajouterBatimentsZC_Ville ville =
	let vilmap = getVilleMap ville in 
	let zrs = filterZCs vilmap in 
	let filtered_twice = checkConditions zrs in
	let zip_zrs = zip (Map.keys filtered_twice) (Map.elems filtered_twice) in
	let withBuildings = map (\(k,v) -> (k,ajouterBatimentsZC v)) zip_zrs in
	let vilmap' = foldr (\(k,v) acc -> Map.insert k v acc) vilmap withBuildings in  
	let vil' = updateVille_viZ ville vilmap'
	in vil' 
	where
		checkConditions zones_res = Map.filter (adjacente_a_route ville) zones_res


--------------------------------------------------

type GameRunner = (GameState,Int, Int)

newGameRunner::GameRunner
newGameRunner= (initGameState,0,0)

getGameStatefromGameRunner::GameRunner->GameState
getGameStatefromGameRunner (gs,_,_) = gs

getInstantsfromGameRunner::GameRunner->(Int,Int)
getInstantsfromGameRunner (_,current,counter) = (current,counter)

updateGameStateinGameRunner::GameRunner->GameState->GameRunner
updateGameStateinGameRunner (_,current,counter) gs = (gs,current,counter)

-- updateGameRunner::GameRunner -> GameRunner  
-- updateGameRunner (gs,current,x) = if x>1000 
-- 	then (updategs gs,current+1,0) 
-- 	else (gs,current,x+1)
-- 	where 
--         updategs gamestate =
--             let v = checkVille $ getVille gamestate
--                 gs' = updateGameState_ville gamestate v
--                 gsFinal = etape gs'
--             in gsFinal

updateGameRunner::GameRunner -> GameRunner 
updateGameRunner (gs, current, x) =
    if x > 500
        then (updategs gs, current + 1, 0)
        else (gs, current, x + 1)
    where
        updategs gamestate =
            let gamestate' = etape gamestate
                (ville', joueur') = if current `mod` 3 == 0
                                    then collecterImpotsCitoyens (gameMap gamestate') (player gamestate') 
                                    else (gameMap gamestate', player gamestate')
            in updateGameState_ville (updateGameState_player gamestate' joueur') (checkVille ville')

---add do notation
checkVille::Ville->Ville
checkVille v = 
	let v' = ajouterBatimentsZR_Ville v in
	let v'' = ajouterBatimentsZI_Ville v' in 
	let v3 = ajouterBatimentsZC_Ville v'' in 
	v3

---
getAllBuildings::GameState->[Batiment]
getAllBuildings gs =
	let zones = Map.elems $ getVilleMap_gs gs in 
	let batlists=[b |b<-(map getBatiments zones),(length b)>0] in
	foldr (\x acc -> x<>acc) [] batlists

filterBuildingsZR::GameState -> [Batiment]
filterBuildingsZR gs =
	let zones = Map.elems $ filterZRs $ getVilleMap_gs gs in 
	let batlists=[b |b<-(map getBatiments zones),(length b)>0] in
	foldr (\x acc -> x<>acc) [] batlists

filterBuildingsZC::GameState -> [Batiment]
filterBuildingsZC gs =
	let zones = Map.elems $ filterZCs $ getVilleMap_gs gs in 
	let batlists=[b |b<-(map getBatiments zones),(length b)>0] in
	foldr (\x acc -> x<>acc) [] batlists

filterBuildingsZI::GameState -> [Batiment]
filterBuildingsZI gs =
	let zones = Map.elems $ filterZIs $ getVilleMap_gs gs in 
	let batlists=[b |b<-(map getBatiments zones),(length b)>0] in
	foldr (\x acc -> x<>acc) [] batlists

getAllCitizens::GameState->Map CitId Citoyen
getAllCitizens gs =
	let Ville{viZones=_,viCit=vicit}= getVille gs in
	vicit

---simulations for citizens--
etape :: GameState -> GameState
etape gs@(GameState { gameMap = ville, player = joueur }) =
	let villeApresImmigration = gererImmigration ville in
    let villeApresDeplacement = deplacerCitoyens villeApresImmigration in
    let villeApresMiseAJourEtats = miseAJourEtatsCitoyens villeApresDeplacement in
	let villeApresAugmentationFaim = augmenterFaimCitoyens villeApresMiseAJourEtats in
    let villeFinale = gererEmigration villeApresAugmentationFaim
    in gs { gameMap = villeFinale }



-- ELECTRICITE CHECKS

electricPathExists::Ville->Coord->Coord->Bool
electricPathExists v src dst =
	case astarPathfinding' src dst v of
		Just x -> True
		Nothing -> False

getCentralesLocations::Ville->[Coord]
getCentralesLocations v =
	let centrales = filterCentrales $ getVilleMap v in
	let centraleCoords = foldr (\x acc -> x <> acc) [] $ map (formeToCoords . zoneForme) $ Map.elems centrales in
	centraleCoords

filterConnectedZones::Ville->Zone->Bool
filterConnectedZones v z =
	let centralecoords = getCentralesLocations v in 
	let zonecoords = (formeToCoords . zoneForme) z in
	let all2all = [(x,y)| x<-centralecoords,y<-zonecoords] in 
	let connected=map (\(x,y)->electricPathExists v x y) all2all in
	any (==True) connected 

---- ELECTRICITE PATHFINDING

astarPathfinding' :: Coord -> Coord -> Ville -> Maybe [Coord]
astarPathfinding' start end ville = aStar (Set.singleton (Node start 0 (heuristic start end))) Set.empty Map.empty where
    aStar reste visited cameFrom
        | Set.null reste = Debug.Trace.trace "Open set is empty, no path found" Nothing
        | coord currentNode == end = Debug.Trace.trace ("Path found: " ++ show (buildPath2 cameFrom end)) (Just (buildPath2 cameFrom end))
        | otherwise = Debug.Trace.trace ("Current node: " ++ show currentNode) $
            let
                reste' = Set.delete currentNode reste
                visited' = Set.insert (coord currentNode) visited
                neighbors = filter (`Set.notMember` visited') $ getValidNeighbors' (coord currentNode) ville
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

buildPath2 :: Map.Map Coord Coord -> Coord -> [Coord]
buildPath2 cameFrom current = buildPath3 cameFrom current []

buildPath3 :: Map.Map Coord Coord -> Coord -> [Coord] -> [Coord]
buildPath3 cameFrom current path =
    case Map.lookup current cameFrom of
        Just prev -> buildPath3 cameFrom prev (current : path)
        Nothing -> current : path

getValidNeighbors' :: Coord -> Ville -> [Coord]
getValidNeighbors' (C x y) ville = Debug.Trace.trace ("Getting neighbors for " ++ show (C x y)) $
    filter (isValidCoord' ville) [C (x + 1) y, C (x - 1) y, C x (y + 1), C x (y - 1)]


isValidCoord' :: Ville -> Coord -> Bool
isValidCoord' ville coord = Debug.Trace.trace ("Checking if " ++ show coord ++ " is valid") $
    case trouveZoneIdParCoord coord ville of
        Just zoneId -> case Map.lookup zoneId (viZones ville) of
            Just zone -> isTraversable' zone
            Nothing -> Debug.Trace.trace ("No zone found for coord: " ++ show coord) False
        Nothing -> Debug.Trace.trace ("No zoneId found for coord: " ++ show coord) False

isTraversable' :: Zone -> Bool
isTraversable' (Cable _) = True
isTraversable' _ = False