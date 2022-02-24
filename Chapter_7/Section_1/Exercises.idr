module Chapter_7.Section_1.Exercises

import Chapter_4.Shape
import Data.List

Eq Shape where
    Triangle d1 d2 == Triangle d1' d2' = d1 == d1' && d2 == d2'
    Rectangle d1 d2 == Rectangle d1' d2' = d1 == d1' && d2 == d2'
    Circle d == Circle d' = d == d'
    _ == _ = False

Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]