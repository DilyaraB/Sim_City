module Types where

import Data.Map (Map)

data Coord = C { cx :: Int, cy :: Int }
  deriving (Show, Eq, Ord)

data Forme = HSegment Coord Int
           | VSegment Coord Int
           | Rectangle Coord Int Int
  deriving (Show, Eq)

data Batiment = Cabane BatId Forme Coord Int [CitId]
              | Atelier BatId Forme Coord Int [CitId]
              | Epicerie BatId Forme Coord Int [CitId]
              | Commissariat BatId Forme Coord
  deriving (Show, Eq)

data Zone = Eau Forme
          | Vide Forme
          | Route Forme
          | ZR Forme [Batiment]
          | ZI Forme [Batiment]
          | ZC Forme [Batiment]
          | Admin Forme Batiment
          | Centrale Forme
          | Cable Forme
  deriving (Show, Eq)

newtype ZoneId = ZoneId Int
  deriving (Show, Eq, Ord)

newtype BatId = BatId String
  deriving (Show, Eq, Ord)

newtype CitId = CitId String
  deriving (Show, Eq, Ord)

data Ville = Ville {
  viZones :: Map ZoneId Zone,
  viCit :: Map CitId Citoyen
} deriving (Show, Eq)

data Citoyen = 	  Immigrant Coord (Int,Int,Int) Occupation
				| Habitant Coord (Int,Int,Int) (BatId, Maybe BatId, Maybe BatId) Occupation
				| Emigrant Coord Occupation
	deriving (Show,Eq)

data Occupation= Travail
				| Dodo
				| Courses
				| Deplacement Coord
	deriving (Show,Eq)

type Cost = Int

data Node = Node {
    coord :: Coord,
    costFromStart :: Cost,
    estimatedTotalCost :: Cost
} deriving (Eq, Show)

instance Ord Node where
    compare a b = compare (estimatedTotalCost a) (estimatedTotalCost b)