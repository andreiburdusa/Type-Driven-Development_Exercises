module Chapter_7.Section_2.Exercises

import Chapter_7.Section_2.Expr

-- Exercise 1

Show num => Show (Expr num) where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Abs x) = "( -" ++ show x ++ ")"

-- Exercise 2

(Neg num, Integral num, Abs num, Eq num) => Eq (Expr num) where
    x == y = eval x == eval y

-- Exercise 3

(Neg num, Integral num, Abs num) => Cast (Expr num) num where
    cast e = eval e