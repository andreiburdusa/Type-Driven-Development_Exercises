module Chapter_6.Section_1.TypeSynonym

import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position 

tri : Polygon 3
tri = (?tri_rhs_10, ?tri_rhs_11) :: ((?tri_rhs_1, ?tri_rhs_2) :: [(?tri_rhs_7, ?tri_rhs_8)])
