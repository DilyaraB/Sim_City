{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import Types
import Data.Map (Map, elems)
import qualified Data.Map as Map
import Zones

import Model
import SDL
import SDL.Time (time, delay)
import SDL.Input.Mouse
import qualified SDL.Font as Font
import SDL.Font (Font)
import Linear (V4(..))
import qualified Data.Text as T

import Affichage.TextureMap (TextureMap, TextureId (..))
import qualified Affichage.TextureMap as TM

import Affichage.Sprite (Sprite)
import qualified Affichage.Sprite as S

import Affichage.SpriteMap (SpriteMap, split, SpriteId (..))
import qualified Affichage.SpriteMap as SM

import Input.Mouse
import qualified Input.Mouse as M

import Input.Keyboard
import qualified Input.Keyboard as K

import GHC.Int

-- loadBackground::Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
-- loadBackground rdr path tmap smap = do
--   tmap' <- TM.loadTexture rdr path (TextureId "vide") tmap
--   let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "vide") (S.mkArea 0 0 30 30)
--   let smap' = SM.addSprite (SpriteId "vide") sprite smap
--   return (tmap', smap')

-- loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
-- loadPerso rdr path tmap smap = do
--   tmap' <- TM.loadTexture rdr path (TextureId "eau") tmap
--   let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "eau") (S.mkArea 0 0 30 30)
--   let smap' = SM.addSprite (SpriteId "eau") sprite smap
--   return (tmap', smap')

fetchRightTexture :: Coord -> GameState -> TextureId
fetchRightTexture co gs@(GameState{mode=m,coordMap=cmap,gameMap=v,player=j}) =
	let tilemap = getVilleMap v in 
		case Map.lookup co cmap of
			Just zid -> case Map.lookup zid tilemap of
				Just (Vide f) -> TextureId "vide"
				Just (Eau f) -> TextureId "eau"
				otherwise -> TextureId ""
			otherwise -> TextureId ""

addCitizenSprite :: Coord -> SpriteMap -> SpriteMap
addCitizenSprite (C x y) smap =
	let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "sim") (S.mkArea 0 0 30 30) in
	let spriteId = (SpriteId ("H"<>(show x)<>","<>(show y))) in
	let smap' = Map.insert spriteId sprite smap in
	smap'


loadOneSprite :: Coord -> SpriteMap -> SpriteMap
loadOneSprite (C x y) smap =
	let sprite = 
		S.defaultScale 
		$ S.addImage S.createEmptySprite 
		$ S.createImage (TextureId "vide") (S.mkArea (fromIntegral x*30) (fromIntegral y*30) 30 30) in
	let smap' = 
		Map.insert 
		(SpriteId ("T"<>(show x)<>","<>(show y))) sprite smap in
	smap'

floodSprite :: Coord -> SpriteMap -> SpriteMap
floodSprite (C x y) smap =
	let sprite = 
		S.defaultScale 
		$ S.addImage S.createEmptySprite 
		$ S.createImage (TextureId "eau") (S.mkArea (fromIntegral x*30) (fromIntegral y*30) 30 30) in
	let smap' = 
		SM.changeSprite
		(SpriteId ("T"<>(show x)<>","<>(show y))) sprite smap in
	smap'

floodSpriteLine :: [Coord] -> SpriteMap -> SpriteMap
floodSpriteLine cos smap =
	foldr (\x acc -> floodSprite x acc) smap cos

spriteGen::Coord->TextureId->Sprite
spriteGen (C x y) tid =
	S.defaultScale 
	$ S.addImage S.createEmptySprite 
	$ S.createImage tid (S.mkArea (fromIntegral x*30) (fromIntegral y*30) 30 30)

updateBuildingSprites::GameState->SpriteMap->SpriteMap
updateBuildingSprites gs smap =
	let resBuildingCoordList = (map batimentStartingCoord $ filterBuildingsZR gs) in 
	let indBuildingCoordList = (map batimentStartingCoord $ filterBuildingsZI gs ) in 
	let comBuildingCoordList = (map batimentStartingCoord $ filterBuildingsZC gs) in
	let zrsmap = foldr (\co acc -> updateTileFromCoord co (TextureId "cabane") acc) smap resBuildingCoordList in
	let zismap = foldr (\co acc -> updateTileFromCoord co (TextureId "atelier") acc) zrsmap indBuildingCoordList in
	let zcsmap = foldr (\co acc -> updateTileFromCoord co (TextureId "epicerie") acc) zismap comBuildingCoordList in
	zcsmap


updateTileFromCoord::Coord -> TextureId -> SpriteMap -> SpriteMap
updateTileFromCoord (C x y) tid smap =
	SM.changeSprite (SpriteId ("T"<>(show x)<>","<>(show y))) (spriteGen (C x y) tid) smap

updateTileFromZone::Zone->SpriteMap->SpriteMap
updateTileFromZone (Route (VSegment (C x y) l)) smap = 
	foldr (\co acc -> updateTileFromCoord co (TextureId "VRoad") acc) smap (formeToCoords (VSegment (C x y) l))
updateTileFromZone (Route (HSegment (C x y) l)) smap = 
	foldr (\co acc -> updateTileFromCoord co (TextureId "HRoad") acc) smap (formeToCoords (HSegment (C x y) l))
updateTileFromZone (Cable (VSegment (C x y) l)) smap = 
	foldr (\co acc -> updateTileFromCoord co (TextureId "VCable") acc) smap (formeToCoords (VSegment (C x y) l))
updateTileFromZone (Cable (HSegment (C x y) l)) smap = 
	foldr (\co acc -> updateTileFromCoord co (TextureId "HCable") acc) smap (formeToCoords (HSegment (C x y) l))
updateTileFromZone (Eau (Types.Rectangle (C x y) l w)) smap = 
	foldr (\co acc -> updateTileFromCoord co (TextureId "eau") acc) smap (formeToCoords (Types.Rectangle (C x y) l w))
updateTileFromZone _ smap = smap

updateAllTiles::GameState->SpriteMap->SpriteMap
updateAllTiles gs smap = 
	let nonEmptyZones=Map.elems (filterEmptyZones_gs gs) in
	foldr (\x acc -> updateTileFromZone x acc) smap nonEmptyZones

loadAllSprites::GameState -> SpriteMap
loadAllSprites gs@(GameState{mode=m,coordMap=cmap,gameMap=v,player=j}) =
	let clist = Map.keys cmap in 
	let foldedsmap = foldr (\c acc -> loadOneSprite c acc) SM.createSpriteMap clist in 
	foldedsmap

loadAllCitizenSprites::GameState->SpriteMap
loadAllCitizenSprites gs =
	let citpositions = getAllCitizenPositions gs in
	let foldedsmap = foldr (\c acc -> addCitizenSprite c acc) SM.createSpriteMap citpositions in
	foldedsmap

-- TODO List with paths and fold instead of going up to tmap913984134
loadAllTextures::Renderer -> TextureMap -> IO (TextureMap)
loadAllTextures renderer tmap =
	do
	tmap' <- TM.loadTexture renderer "assets/empty_tile.bmp" (TextureId "vide") tmap
	tmap'' <- TM.loadTexture renderer "assets/water.bmp" (TextureId "eau") tmap'
	tmap3 <- TM.loadTexture renderer "assets/residential_zone.bmp" (TextureId "RZ") tmap''
	tmap4 <- TM.loadTexture renderer "assets/vroad.bmp" (TextureId "VRoad") tmap3
	tmap5 <- TM.loadTexture renderer "assets/hroad.bmp" (TextureId "HRoad") tmap4
	tmap6 <- TM.loadTexture renderer "assets/cabane.bmp" (TextureId "cabane") tmap5
	tmap7 <- TM.loadTexture renderer "assets/centrale.bmp" (TextureId "Centrale") tmap6
	tmap8 <- TM.loadTexture renderer "assets/HCable.bmp" (TextureId "HCable") tmap7
	tmap9 <- TM.loadTexture renderer "assets/VCable.bmp" (TextureId "VCable") tmap8
	tmap10 <- TM.loadTexture renderer "assets/industrial_zone.bmp" (TextureId "IZ") tmap9
	tmap11 <- TM.loadTexture renderer "assets/commercial_zone.bmp" (TextureId "CZ") tmap10
	tmap12 <- TM.loadTexture renderer "assets/atelier.bmp" (TextureId "atelier") tmap11
	tmap13 <- TM.loadTexture renderer "assets/epicerie.bmp" (TextureId "epicerie") tmap12
	tmap14 <- TM.loadTexture renderer "assets/sim.bmp" (TextureId "sim") tmap13
	return tmap14

addOverlaySprite :: GameState -> Coord -> SpriteMap -> SpriteMap
addOverlaySprite gs co smap =
	case Map.lookup co (getCMap gs) of
		Just zid -> case Map.lookup zid (getVilleMap_gs gs) of
			Just (ZR _ _) -> addZRSprite co smap
			Just (ZI _ _) -> addZISprite co smap
			Just (ZC _ _) -> addZCSprite co smap
			Just (Centrale _) -> addCentraleSprite co smap
			_ -> smap
		Nothing -> smap

----Factorise into single function
addZRSprite :: Coord -> SpriteMap-> SpriteMap
addZRSprite (C x y) smap = 
	let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "RZ") (S.mkArea 0 0 90 90) in
	let spriteId = (SpriteId ("O"<>(show x)<>","<>(show y))) in
	let smap' = Map.insert spriteId sprite smap in
	smap'

addZCSprite :: Coord -> SpriteMap-> SpriteMap
addZCSprite (C x y) smap = 
	let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "CZ") (S.mkArea 0 0 90 90) in
	let spriteId = (SpriteId ("O"<>(show x)<>","<>(show y))) in
	let smap' = Map.insert spriteId sprite smap in
	smap'

addZISprite :: Coord -> SpriteMap-> SpriteMap
addZISprite (C x y) smap = 
	let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "IZ") (S.mkArea 0 0 90 90) in
	let spriteId = (SpriteId ("O"<>(show x)<>","<>(show y))) in
	let smap' = Map.insert spriteId sprite smap in
	smap'

addCentraleSprite :: Coord -> SpriteMap-> SpriteMap
addCentraleSprite (C x y) smap = 
	let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Centrale") (S.mkArea 0 0 90 90) in
	let spriteId = (SpriteId ("O"<>(show x)<>","<>(show y))) in
	let smap' = Map.insert spriteId sprite smap in
	smap'

---GENERALIZE later (forme ...)
-- addOverlaySprite::Zone->SpriteMap->SpriteMap
-- addOverlaySprite (ZR _ _) smap = Map.insert 

updateOverlay::GameState->SpriteMap->SpriteMap
updateOverlay gs smap = 
	let olays = filterOverlays_gs gs in
	foldr (\x acc -> addOverlaySprite gs (formeStartingPoint (zoneForme x)) acc) smap olays   


combineSpriteTextureMap::SpriteMap->IO (TextureMap) -> IO (TextureMap,SpriteMap)
combineSpriteTextureMap smap tmap =
	do
	tmap' <- tmap
	return (tmap',smap)


coord2TextId :: Coord -> TextureId
coord2TextId (C x y) =
	TextureId ("T"<>(show x)<>","<>(show y))

-- tileSid = zip (Map.keys cmap) (map (\(C x y) -> "T"<>(show x)<>","<>(show y)) (Map.keys cmap))
	
displayAllSprites:: Renderer -> TextureMap -> SpriteMap -> GameState -> IO()
displayAllSprites renderer tmap smap gs@(GameState{mode=m,coordMap=cmap,gameMap=v,player=j}) =
	let provisoire = zip (Map.keys smap) [(CInt 30*x, CInt 30*y)|x<-[0..30],y<-[0..30]] in
	-- let coordAndTile=zip (Map.keys cmap) (map (\x -> fetchRightSprite x gs) (Map.keys cmap)) in
	mapM_ (displayTileCoord) (provisoire)
	where 
		displayTileCoord (s,(x,y)) = S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (s) smap) 
																		x 
																		y)
displayAllCitizenSprites::Renderer->TextureMap->SpriteMap->IO()
displayAllCitizenSprites renderer tmap smap =
	let coordList=[a | a<-(map coordFromSpriteId (Map.keys smap))] in
	let provisoire = zip (Map.keys smap) (coordList) in
	mapM_ (displayCitSprite) (provisoire)
	where
	displayCitSprite (s,(x,y)) = S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (s) smap) (x*30) (y*30))

coordFromSpriteId::SpriteId->(CInt,CInt)
coordFromSpriteId (SpriteId a) =
    let a' = Affichage.SpriteMap.split ',' (tail a) in
    let ax = read (head a')::CInt in
    let ay = read (last a')::CInt in
    (ax, ay)


displayAllOverlays::Renderer -> TextureMap -> SpriteMap -> IO()
displayAllOverlays renderer tmap smap =
	let coordList=[a |a<-(map coordFromSpriteId (Map.keys smap))] in
	let provisoire = zip (Map.keys smap) (coordList) in
	mapM_ (displayOverlay) (provisoire)
	where 
	displayOverlay (s,(x,y)) = S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (s) smap) (x*30) (y*30)) 

-- Fetch right tile according to Zone/BuildId etc
-- fetchRightTile:: GameState -> Sprite

-- displayFromMap:: Renderer -> TextureMap -> Sprite -> GameState -> IO () 

getMouseClick::[Event]->Mouse->Maybe(Int32,Int32)
getMouseClick events mouse = 
	if length events > 0
	then	M.handleMouseEvent' (last events) mouse
	else Nothing

handleClickDisplay::Maybe(Int32,Int32) -> SpriteMap -> SpriteMap
handleClickDisplay co sm =
	case co of
		Just (a,b) -> floodSprite (C (fromIntegral a) (fromIntegral b)) sm
		Nothing -> sm

-- handleClickInternal::Maybe(Int32,Int32) -> GameState -> GameState 
-- handleClickInternal co gs@(GameState{mode=gm}) =
-- 	case gm of
-- 		Lining -> case co of
-- 				Just (a,b) ->  
-- 				Nothing -> gs 
-- 		Build -> case co of
-- 				Just (a,b) -> convert2water' (C (fromIntegral a) (fromIntegral b)) gs
-- 				Nothing -> gs
-- 		View -> gs

handleClickInternal::Maybe(Int32,Int32) -> GameState -> GameState 
handleClickInternal co gs@(GameState{mode=gm}) =
	case co of
	Just (a,b) -> 
		let x = fromIntegral a in
		let y = fromIntegral b in
		case gm of 
			View -> gs
			Build -> convert2water' (C x y) gs
			Lining Unset Unset -> gs{mode=(Lining (Ending (C x y)) Unset)}
			Lining (Ending p1) Unset -> updateGameState_mode (insertRoads (roadShapeGuesser p1 (C x y)) gs) (Lining Unset Unset)
			BuildCentrale -> insertCentrale (C x y) gs
			BuildCable Unset Unset -> gs{mode=(BuildCable (Ending (C x y)) Unset)}
			BuildCable (Ending p1) Unset -> updateGameState_mode (insertCables (roadShapeGuesser p1 (C x y)) gs) (BuildCable Unset Unset)
			BuildZR -> insertZR (C x y) gs
			BuildZI -> insertZI (C x y) gs
			BuildZC -> insertZC (C x y) gs
			_ -> gs
	Nothing -> gs


updateState::Keyboard->GameState->GameState
updateState kbd gs =
	if (K.keypressed KeycodeC kbd) then toggleBuildZC gs else 
	if (K.keypressed KeycodeI kbd) then toggleBuildZI gs else 
	if (K.keypressed KeycodeL kbd) then toggleLining gs else
	if (K.keypressed KeycodeR kbd) then toggleBuildZR gs else 
	if (K.keypressed KeycodeZ kbd) then toggleBuildCentrale gs else
	if (K.keypressed KeycodeE kbd) then toggleBuildCable gs
	else gs

main :: IO ()
main = do
	initializeAll
	let gameRunner = newGameRunner 
	let gameState = getGameStatefromGameRunner gameRunner
	window <- createWindow "SimCity" $ defaultWindow { windowInitialSize = V2 1200 1200 }
	renderer <- createRenderer window (-1) defaultRenderer
	tmap <- loadAllTextures renderer TM.createTextureMap
  	

	Font.initialize
	font <- Font.load "assets/Roboto-regular.ttf" 17
	let textColor = V4 255 255 255 255
	textSurface <- Font.blended font textColor "Test"
	textTexture <- createTextureFromSurface renderer textSurface
	TextureInfo {textureWidth=textWidth, textureHeight = textHeight} <- queryTexture textTexture
	


  	let smap = loadAllSprites gameState
  	let overlayMap = SM.createSpriteMap

	putStrLn "ok"
	let mouse = M.createMouse
	let keyboard = K.createKeyboard
	
	gameLoop 60 renderer tmap font smap overlayMap keyboard mouse gameRunner

renderText :: Renderer -> Font -> V2 CInt -> T.Text -> IO()
renderText renderer font (V2 x y) text = do 
	let textColor = V4 255 255 255 255
	textSurface <- Font.blended font textColor text
	textTexture <- createTextureFromSurface renderer textSurface
	TextureInfo { textureWidth = textWidth, textureHeight = textHeight } <- queryTexture textTexture
	copy renderer textTexture Nothing (Just (SDL.Rectangle (P (V2 x y)) (V2 (fromIntegral textWidth) (fromIntegral textHeight))))
 

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> Font->SpriteMap -> SpriteMap -> Keyboard -> Mouse -> GameRunner -> IO ()
gameLoop frameRate renderer tmap font smap overlayMap kbd mouse gameRunner = do
	let gameState = getGameStatefromGameRunner gameRunner

	startTime <- time
	events <- pollEvents
	let kbd' = K.handleEvents events kbd
	let gameState' = updateState kbd' gameState 

	if (length events>0) then (M.handleMouseEvent (last events) mouse) else pure ()
	let click = getMouseClick events mouse

	let gameState'' = handleClickInternal click gameState'
	let smap' = (updateBuildingSprites gameState'' (updateAllTiles gameState'' smap))
	let overlayMap'=updateOverlay gameState overlayMap

	clear renderer
	let citsmap = loadAllCitizenSprites gameState''

	displayAllSprites renderer tmap smap gameState
	displayAllOverlays renderer tmap overlayMap
	displayAllCitizenSprites renderer tmap citsmap 

	renderText renderer font (V2 (31*30) (20))  (T.pack ("Mode: "<> (displayGameMode gameState''))) 
	renderText renderer font (V2 (31*30) (80))  (T.pack ("Population: "<> (show $ length $ Data.Map.elems $ getAllCitizens gameState''))) 
	renderText renderer font (V2 (31*30) (120)) (T.pack ("Argent: $"<> (show $ getJoueurFunds gameState''))) 
	renderText renderer font (V2 (31*30) (240)) (T.pack "Choisir sommet:")
	renderText renderer font (V2 (31*30) (280)) (T.pack "R: Zone Residentielle $100")
	renderText renderer font (V2 (31*30) (320)) (T.pack "I: Zone Industrielle $300")
	renderText renderer font (V2 (31*30) (360)) (T.pack "C: Zone Commercialle $200")
	renderText renderer font (V2 (31*30) (400)) (T.pack "Z: Centrale Electrique $500")
	renderText renderer font (V2 (31*30) (500)) (T.pack "Choisir depart et fin:")
	renderText renderer font (V2 (31*30) (540)) (T.pack "L: Route $10 par case")
	renderText renderer font (V2 (31*30) (580)) (T.pack "E: Cable electrique $10 par case")


	present renderer

	let gameRunner' = updateGameRunner (updateGameStateinGameRunner gameRunner gameState'')

	-- putStrLn (show $ getAllBuildings gameState'')
	putStrLn (show $ getAllCitizens gameState'')

	endTime <- time
	let refreshTime = endTime - startTime
	let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
	threadDelay $ delayTime * 1000

	unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap font smap' overlayMap' kbd' mouse gameRunner')


