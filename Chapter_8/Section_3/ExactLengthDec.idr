module Chapter_8.Section_3.ExactLengthDec

import Data.Vect
import Decidable.Equality

exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input =
    case decEq m len of
        Yes Refl => Just input
        No _ => Nothing