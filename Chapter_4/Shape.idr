module Chapter_4.Shape

||| Represents shapes
public export -- The name, type, and complete definition are exported.
data Shape  = ||| A triangle, with its base length and height
                Triangle Double Double
            | ||| A rectangle, with its length and height
                Rectangle Double Double
            | ||| A circle, with its radius
                Circle Double

export -- The name and type are exported, but not the definition.
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Shape' : Type where
    Triangle' : Double -> Double -> Shape'
    Rectangle' : Double -> Double -> Shape'
    Circle' : Double -> Shape'
