module TryIndex

import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs =
  case integerToFin i n of
    Nothing => Nothing
    (Just idx) => Just (index idx xs)

vectTake : (quantity : Fin n) -> Vect n elem -> Vect (finToNat quantity) elem
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

-- test1 = vectTake 3 [1,2,3,4,5,6,7]
-- test2 = vectTake 8 [1,2,3,4,5,6,7]

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing => Nothing
    (Just idx) => Just (index idx xs + index idx ys)
