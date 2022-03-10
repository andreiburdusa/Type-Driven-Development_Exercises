module Chapter_10.Section_3.Shape_abs

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

-- Needed for Chapter_10.Section_3.Exercises
public export
data ShapeView : Shape -> Type where
    STriangle : {base:_} -> {height:_} -> ShapeView (triangle base height)
    SRectangle : {width:_} -> {height:_} -> ShapeView (rectangle width height)
    SCircle : {radius:_} -> ShapeView (circle radius)

export
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle _ _) = STriangle
shapeView (Rectangle _ _) = SRectangle
shapeView (Circle _) = SCircle
