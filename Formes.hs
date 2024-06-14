module Formes where

import Types
import Debug.Trace

-- calcule les bords des formes
limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y, y + l - 1, x, x)
limites (Rectangle (C x y) w h) = (y, y + h - 1, x, x + w - 1)

--verifie si Coord se trouve a l'interieur d'une forme
appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x0 y0) l) = y == y0 && x >= x0 && x < x0 + l
appartient (C x y) (VSegment (C x0 y0) l) = x == x0 && y >= y0 && y < y0 + l
appartient (C x y) (Rectangle (C x0 y0) w h) = x >= x0 && x < x0 + w && y >= y0 && y < y0 + h

appartient' :: Forme -> Coord -> Bool
appartient' (HSegment (C x0 y0) l) (C x y)  = y == y0 && x >= x0 && x <= x0 + l
appartient' (VSegment (C x0 y0) l) (C x y) = x == x0 && y >= y0 && y <= y0 + l
appartient' (Rectangle (C x0 y0) w h) (C x y) = x >= x0 && x <= x0 + w && y >= y0 && y <= y0 + h

--verifie si il y a une collision entre 2 formes
collision_approx :: Forme -> Forme -> Bool
collision_approx f1 f2 =
  let (n1, s1, o1, e1) = limites(f1)
      (n2, s2, o2, e2) = limites(f2)
      overlap = not (e1 < o2 || e2 < o1 || s1 < n2 || s2 < n1)
  in traceShow ("Comparing:", f1, "with", f2, "Limits:", (n1, s1, o1, e1), (n2, s2, o2, e2), "Overlap:", overlap) overlap

