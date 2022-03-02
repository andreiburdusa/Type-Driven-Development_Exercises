module Chapter_8.Section_3.Exercise1

import Data.Vect
import Decidable.Equality

-- Exercise 1
headUnequal :
    DecEq a =>
    {xs : Vect n a} ->
    {ys : Vect n a} ->
    (contra : (x = y) -> Void) ->
    ((x :: xs) = (y :: ys)) ->
    Void
headUnequal {y = x} contra Refl = contra Refl

tailUnequal :
    DecEq a =>
    {xs : Vect n a} ->
    {ys : Vect n a} ->
    (contra : (xs = ys) -> Void) ->
    ((x :: xs) = (y :: ys)) ->
    Void
tailUnequal {ys = xs} contra Refl = contra Refl

    