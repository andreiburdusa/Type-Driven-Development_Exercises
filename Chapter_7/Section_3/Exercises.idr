module Chapter_7.Section_3.Exercises

import Chapter_7.Section_2.Expr

-- Exercise 1
Functor Expr where
    map f (Val x) = Val (f x)
    map f (Add x y) = Add (map f x) (map f y)
    map f (Sub x y) = Sub (map f x) (map f y)
    map f (Mul x y) = Mul (map f x) (map f y)
    map f (Div x y) = Div (map f x) (map f y)
    map f (Abs x) = Abs (map f x)

-- Exercise 2

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq a => Eq (Vect len a) where
    [] == [] = True
    (x :: xs) == (y :: ys) = x == y && xs == ys

Foldable (Vect len) where
    foldr f unit [] = unit
    foldr f unit (x :: xs) = f x (foldr f unit xs)


