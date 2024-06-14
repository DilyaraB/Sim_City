module Input.Mouse where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S
import GHC.Int

type Mouse = Set (Point V2 Int32)

--cree une structure de souris vide
createMouse:: Mouse
createMouse = S.empty

--handle event de souris, affiche les coordonnes de clic 
handleMouseEvent:: Event -> Mouse -> IO()
handleMouseEvent event mouse =
 	case eventPayload event of
 		MouseButtonEvent mouseButtonEvent -> 
 			if mouseButtonEventMotion mouseButtonEvent == Pressed 
 			then (if mouseButtonEventButton mouseButtonEvent == ButtonLeft
		 		 then putStrLn (show ((\(P (V2 x y)) -> ( (x `div` 30), (y `div` 30))) (mouseButtonEventPos mouseButtonEvent)))
		 		 else pure ())
	 		else pure ()
 		_ -> pure ()

--handle event de souris, retourne les coordonnes de clic
handleMouseEvent':: Event -> Mouse -> Maybe (Int32,Int32)
handleMouseEvent' event mouse =
 	case eventPayload event of
 		MouseButtonEvent mouseButtonEvent -> 
 			if mouseButtonEventMotion mouseButtonEvent == Pressed 
 			then (if mouseButtonEventButton mouseButtonEvent == ButtonLeft
		 		 then (Just ((\(P (V2 x y)) -> ( (x `div` 30), (y `div` 30))) (mouseButtonEventPos mouseButtonEvent)))
		 		 else Nothing)
	 		else  Nothing
 		_ ->  Nothing