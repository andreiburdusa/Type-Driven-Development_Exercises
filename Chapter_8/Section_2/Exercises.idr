module Chapter_8.Section_2.Exercises

import Data.Vect

-- Exercise 1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes n 0 = plusZeroRightNeutral n
myPlusCommutes n (S k) =
    rewrite sym (plusSuccRightSucc n k) in
    (cong S (myPlusCommutes n k))

-- Exercise 2
reverseProof_nil : Vect n' a -> Vect (plus n' 0) a
reverseProof_nil xs = rewrite plusZeroRightNeutral n' in xs

reverseProof_xs :
    Vect (S (plus n' len)) a ->
    Vect (plus n' (S len)) a
reverseProof_xs ys = rewrite sym (plusSuccRightSucc n' len) in ys

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n' a -> Vect m a -> Vect (n'+m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) =
        reverseProof_xs (reverse' (x::acc) xs)