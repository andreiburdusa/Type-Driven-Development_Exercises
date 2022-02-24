module Chapter_8.Section_1.Exercises

-- Exercise 1
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong (x ::) prf

-- Exercise 2
eqPairs : x = y -> x' = y' -> (x, x') = (y, y')
eqPairs {x} {y = x} Refl Refl = ?rhs
-- eqPairs {x = x} {y = x} {x' = x'} {y' = x'} Refl Refl = Refl

same_lists :
    {xs : List a} ->
    {ys : List a} ->
    x = y ->
    xs = ys ->
    x :: xs = y :: ys
same_lists prf prf1 = cong (\(x,xs) => x :: xs) (eqPairs prf prf1)

-- Exercise 3
data ThreeEq : a -> b -> c -> Type where
    Same : (val : a) -> ThreeEq val val val

-- Exercise 4
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (Same x) = Same (S x)


