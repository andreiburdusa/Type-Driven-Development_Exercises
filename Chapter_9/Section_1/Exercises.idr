module Chapter_9.Section_1.Exercises

import Decidable.Equality

-- Exercise 1
data Elem : a -> List a -> Type where
    Here : Elem x (x :: xs)
    There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There later) impossible

notInTail :
    (notHere: value = x -> Void) ->
    (notThere: Elem value xs -> Void) ->
    Elem value (x :: xs) ->
    Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : List a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) =
    case decEq value x of
        Yes Refl => Yes Here
        No notHere =>
            case isElem value xs of
                Yes prf => Yes (There prf)
                No notThere => No (notInTail notHere notThere)

-- Exercise 2
data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1, 2, 3] 3
last123 = LastCons (LastCons LastOne)

notLastInNil : Last [] value -> Void
notLastInNil LastOne impossible
notLastInNil (LastCons prf) impossible

notInSingleton : (x = value -> Void) -> Last [x] value -> Void
notInSingleton notPresent LastOne = notPresent Refl
notInSingleton _ (LastCons LastOne) impossible
notInSingleton _ (LastCons (LastCons prf)) impossible

notInNonemptyTail :
    (Last (x2 :: xs) value -> Void) ->
    Last (x1 :: (x2 :: xs)) value ->
    Void
notInNonemptyTail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastInNil
isLast [x] value =
    case decEq x value of
        Yes Refl => Yes LastOne
        No notPresent => No (notInSingleton notPresent)
isLast (x1 :: x2 :: xs) value =
    case isLast (x2 :: xs) value of
        Yes prf => Yes (LastCons prf)
        No contra => No (notInNonemptyTail contra)



