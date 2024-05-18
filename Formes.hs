module Formes where

data Coord = C { cx :: Int, cy :: Int }
  deriving (Show, Eq)

data Forme = HSegment Coord Int
           | VSegment Coord Int
           | Rectangle Coord Int Int
  deriving (Show, Eq)

-- calcule les bords des formes
limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y, y + l - 1, x, x)
limites (Rectangle (C x y) w h) = (y, y + h - 1, x, x + w - 1)

--verifi si Coord se trouve a l'interieur d'une forme
appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x0 y0) l) = y == y0 && x >= x0 && x < x0 + l
appartient (C x y) (VSegment (C x0 y0) l) = x == x0 && y >= y0 && y < y0 + l
appartient (C x y) (Rectangle (C x0 y0) w h) = x >= x0 && x < x0 + w && y >= y0 && y < y0 + h

--verifie si il y a une collision entre 2 formes
collision_approx :: Forme -> Forme -> Bool
collision_approx f1 f2 =
  let (n1, s1, o1, e1) = limites(f1)
      (n2, s2, o2, e2) = limites(f2)
  in not (e1 < o2 || e2 < o1 || s1 < n2 || s2 < n1)
