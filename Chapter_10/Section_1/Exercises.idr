module Chapter_10.Section_1.Exercises

import Data.List
import Data.Nat

-- Exercis 1
data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (n_xs : List a) -> {rest : _} -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN 0 xs = Exact [] {rest = xs}
takeN (S k) [] = Fewer
takeN (S k) (y :: ys) =
    case takeN k ys of
        Fewer => Fewer
        Exact n_xs => Exact (y :: n_xs)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
    groupByN n xs | Fewer = [xs]
    groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-- Exercise 2
div2 : Nat -> Nat
div2 k = divNatNZ k 2 SIsNonZero

halves : List a -> (List a, List a)
halves xs with (takeN (div2 (length xs)) xs)
  halves xs | Fewer = ([], xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

