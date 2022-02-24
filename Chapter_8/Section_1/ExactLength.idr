module Chapter_8.Section_1.ExactLength

import Chapter_8.Section_1.EqNat

data Vect : Nat -> Type -> Type where
    Nil : Vect 0 a
    (::) : a -> Vect k a -> Vect (S k) a

exactLength : { m : _ } -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength { m } len input =
    case checkEqNat m len of
        Nothing => Nothing
        Just (Same len) => Just input


