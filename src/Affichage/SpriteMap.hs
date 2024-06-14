module Affichage.SpriteMap where

import Data.Map.Strict (Map)
import qualified Data.Map as M

import Affichage.Sprite (Sprite)
import qualified Affichage.Sprite as S

newtype SpriteId = SpriteId String
  deriving (Eq)

instance Show SpriteId where
  show (SpriteId id) = show id

instance Ord SpriteId where
  (SpriteId a) <= (SpriteId b) =
    let a' = split ',' (tail a) in
    let b' = split ',' (tail b) in
    let ax = read (head a')::Int in
    let ay = read (last a')::Int in
    let bx = read (head b')::Int in 
    let by = read (last b')::Int in
    if (ax==bx) then (ay<=by) else
      (ax<=bx)


type SpriteMap = Map SpriteId Sprite

createSpriteMap :: SpriteMap
createSpriteMap = M.empty

addSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
addSprite sid spr tmap =
  M.insertWithKey (\_ _ _ -> error $ "addSprite - Sprite '" <> (show sid) <> "' already in sprite map.")
  sid spr tmap

fetchSprite :: SpriteId -> SpriteMap -> Sprite
fetchSprite sid smap = case M.lookup sid smap of
                         Nothing -> error $ "fetchSprite - No such Sprite: " <> (show sid)
                         Just spr -> spr

updateSprite :: (Sprite -> Sprite) -> SpriteId -> SpriteMap -> SpriteMap
updateSprite f sid smap = M.alter aux sid smap
  where aux Nothing = error $ "updateSprite - No such sprite '" <> (show sid) <> "' in sprite map."
        aux (Just old) = Just $ f old

changeSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
changeSprite sid spr smap = updateSprite (\_ -> spr) sid smap

removeSprite :: SpriteId -> SpriteMap -> SpriteMap
removeSprite sid smap = case M.lookup sid smap of
                          Nothing -> error $ "removeSprite - No such sprite '" <> (show sid) <> "' in sprite map."
                          Just _ -> M.delete sid smap


-------

splitAux:: Char -> String -> [String]
splitAux splitter string = foldr (\c acc -> if c ==splitter then acc<>[[]] else init acc <> [[c]<>last acc]) [[]] string

split :: Char -> String -> [String]
split splitter string = reverse [x | x <- splitAux splitter string, not (x=="")]

