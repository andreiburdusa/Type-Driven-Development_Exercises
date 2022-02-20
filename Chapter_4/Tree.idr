module Tree

import Picture
import Shape

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr1 expr2) = evaluate expr1 + evaluate expr2
evaluate (Sub expr1 expr2) = evaluate expr1 - evaluate expr2
evaluate (Mult expr1 expr2) = evaluate expr1 * evaluate expr2

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

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
