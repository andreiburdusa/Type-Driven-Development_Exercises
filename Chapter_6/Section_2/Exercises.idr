module Chapter_6.Section_2.Exercises

import Data.Vect

-- Exercise 1
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- Exercise 2: See Printf.idr

-- Exercise 3:
TupleVect : (length : Nat) -> (ty : Type) -> Type
TupleVect 0 ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())

