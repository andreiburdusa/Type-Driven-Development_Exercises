module Chapter_10.Section_2.Exercises

import Data.Vect
import Data.Vect.Views.Extra -- needs contrib
import Data.List.Views.Extra -- needs contrib
import Data.List.Views
import Data.Nat.Views

-- Exercise 1

-- equalSuffix is possibly not terminating
-- (see Type Driven Development with Idris: Updates Required
--      https://idris2.readthedocs.io/en/latest/typedd/typedd.html#chapter-10 )

equalSuffix0 : Eq a => List a -> List a -> List a
equalSuffix0 input1 input2 with (snocList input1, snocList input2)
    equalSuffix0 (xs ++ [x]) (ys ++ [y]) | (Snoc x xs recX, Snoc y ys recY) =
        if x == y then
            x :: (equalSuffix0 xs ys | (recX, recY))
        else
            []
    equalSuffix0 [] input2 | (Empty, y) = []
    equalSuffix0 input1 [] | (x, Empty) = []

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 = reverse $ equalSuffix0 input1 input2

total
equalSuffixTotal : Eq a => List a -> List a -> List a
equalSuffixTotal input1 input2 with (snocList input1)
    equalSuffixTotal [] input2 | Empty = []
    equalSuffixTotal (xs ++ [x]) input2 | Snoc x xs recX with (snocList input2)
        equalSuffixTotal (xs ++ [x]) [] | Snoc x xs recX | Empty = []
        equalSuffixTotal (xs ++ [x]) (ys ++ [y])
          | (Snoc x xs recX)
          | (Snoc y ys recY) =
            case x == y of
                False => []
                True => (equalSuffixTotal xs ys | recX {- | recY -}) ++ [x]

-- Exercise 2

mergeSort : Ord a => {n:_} -> Vect n a -> Vect n a
mergeSort {n} input with (splitRec input)
    mergeSort {n = 0} []
      | SplitRecNil = []
    mergeSort {n = 1} [x]
      | SplitRecOne = [x]
    mergeSort {n = S n' + S m'} .(xs ++ ys)
      | SplitRecPair lrec rrec =    
        merge
            (mergeSort {n = S n'} xs | lrec)
            (mergeSort {n = S m'} ys | rrec)

-- Exercise 3

toBinary : Nat -> String
toBinary k with (halfRec k)
    toBinary 0 | HalfRecZ = "0"
    toBinary (n + n) | HalfRecEven n rec = (toBinary n | rec) ++ "0"
    toBinary (S (n + n)) | HalfRecOdd n rec =
        case (toBinary n | rec) of
            "0" => "1"
            b   => b ++ "1"

-- Exercise 4

palindrome : Eq a => List a -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | VCons rec =
    case x == y of
        False => False
        True => palindrome xs | rec
