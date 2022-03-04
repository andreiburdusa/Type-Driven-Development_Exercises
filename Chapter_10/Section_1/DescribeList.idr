module Chapter_10.Section_1.DescribeList

data ListLast : List a -> Type where
    Empty : ListLast []
    NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) =
    "Non-empty, initial portion = " ++ show xs

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) =
    case listLast xs of
        Empty => NonEmpty [] x
        (NonEmpty ys y) => NonEmpty (x :: ys) y

describeListEnd0 : List Int -> String
describeListEnd0 xs = describeHelper xs (listLast xs)

describeListEnd : List Int -> String
describeListEnd input with (listLast input)
    describeListEnd [] | Empty = "Empty"
    describeListEnd (xs ++ [x]) | (NonEmpty xs x) =
        "Non-empty, initial portion = " ++ show xs

myReverse : List a -> List a
myReverse xs with (listLast xs)
  myReverse [] | Empty = []
  myReverse (ys ++ [x]) | (NonEmpty ys x) = x :: myReverse ys

