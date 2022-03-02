module Chapter_8.Section_3.Exercise2

import Decidable.Equality
import Chapter_8.Section_3.Exercise1

-- Exercise 2
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

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

eqHeadsTails :
    x = y ->
    xs = ys ->
    Dec (Chapter_8.Section_3.Exercise2.(::) x xs
        = Chapter_8.Section_3.Exercise2.(::) y ys)
eqHeadsTails {y = x} {ys = xs} Refl Refl = Yes Refl

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) =
        case decEq x y of
            Yes prf =>
                case decEq xs ys of
                    Yes prf' => eqHeadsTails prf prf'
                    No contra => No (tailUnequal contra)
            No contra => No (headUnequal contra)

