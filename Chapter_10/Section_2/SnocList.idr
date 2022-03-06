module Chapter_10.Section_2.SnocList

import Data.List

data SnocList : List a -> Type where
    Empty : SnocList []
    Snoc : (x, xs : _) -> (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp :
    {input : _} ->
    SnocList input ->
    (xs : List a) ->
    SnocList (input ++ xs)
snocListHelp snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp snoc (x :: xs) =
    rewrite appendAssociative input [x] xs in
        snocListHelp (Snoc x input snoc) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc x xs rec) =
    x :: myReverseHelper xs rec

myReverse0 : List a -> List a
myReverse0 input = myReverseHelper input (snocList input)

myReverse : List a -> List a
myReverse input with (snocList input)
    myReverse [] | Empty = []
    myReverse (xs ++ [x]) | (Snoc x xs rec) = x :: myReverse xs | rec

-- Traversing multiple arguments: nested with blocks

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1, snocList input2)
    isSuffix _ _ | (Snoc x xs xsrec, Snoc y ys ysrec) =
        (x == y) && (isSuffix _ _ | (xsrec, ysrec))
    isSuffix _ _ | (Empty, s) = True
    isSuffix _ _ | (s, Empty) = False