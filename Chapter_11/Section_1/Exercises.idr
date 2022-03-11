module Chapter_11.Section_1.Exercises

import Chapter_11.Section_1.InfList
import Chapter_11.Section_1.Arith
import Data.Stream

-- Exercise 1
every_other : Stream a -> Stream a
every_other (x1 :: x2 :: xs) = x2 :: every_other xs

-- Exercise 2
Functor InfList where
    map f (value :: values) = f value :: map f values

-- Exercise 3
data Face = Heads | Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips 0 xs = []
coinFlips (S k) (x :: xs) = intToFace x :: coinFlips k xs
  where
    intToFace : Int -> Face
    intToFace i = if i `mod` 2 == 0 then Heads else Tails

-- Exercise 4
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number next
  where
    next : Double
    next = (approx + (number / approx)) / 2

-- Exercise 5
square_root_bound :
    (max : Nat) ->
    (number : Double) ->
    (bound : Double) ->
    (approxs : Stream Double) -> Double
square_root_bound 0 number bound (x :: _) = x
square_root_bound (S k) number bound (x :: xs) =
    if diffOk x number then x else square_root_bound k number bound xs
  where
    diffOk : Double -> Double -> Bool
    diffOk val number = abs (val * val - number) < bound

square_root : (number : Double) -> Double
square_root number =
    square_root_bound
        100
        number
        0.00000000001
        (square_root_approx number number)