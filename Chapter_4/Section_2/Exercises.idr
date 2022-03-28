module Chapter_4.Section_2.Exercises

import Data.Vect

-- Exercise 1
-- See Type-Driven-Development_Exercises/Chapter_4/Section_2/vehicle.idr

-- Exercise 2
-- See Type-Driven-Development_Exercises/Chapter_4/Section_2/vehicle.idr

-- Exercises 3, 4
vectTake : (quantity : Fin n) -> Vect n elem -> Vect (finToNat quantity) elem
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

-- Exercise 5

test1 = vectTake 3 [1,2,3,4,5,6,7]
-- test2 = vectTake 8 [1,2,3,4,5,6,7]

sumEntries : Num a => {n:_} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing => Nothing
    (Just idx) => Just (index idx xs + index idx ys)
