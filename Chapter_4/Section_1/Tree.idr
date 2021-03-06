module Chapter_4.Section_1.Tree

import Chapter_4.Section_1.Picture
import Chapter_4.Section_1.Shape

public export
data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

export
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)
