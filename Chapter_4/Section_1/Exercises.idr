module Chapter_4.Section_1.Exercises

import Chapter_4.Section_1.Tree
import Chapter_4.Section_1.Shape
import Chapter_4.Section_1.Picture

-- Exercise 1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Exercise 2
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

-- Exercise 3
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

-- Exercise 4
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr1 expr2) = evaluate expr1 + evaluate expr2
evaluate (Sub expr1 expr2) = evaluate expr1 - evaluate expr2
evaluate (Mult expr1 expr2) = evaluate expr1 * evaluate expr2

-- Exercise 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

-- Exercise 6
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine pic pic1) =
  maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 =
  Combine
  (Primitive (Triangle 2 3))
  (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 =
  Combine
    (Primitive (Rectangle 1 3))
    (Primitive (Circle 4))
