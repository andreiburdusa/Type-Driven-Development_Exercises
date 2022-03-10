module Chapter_10.Section_3.Exercises

import Chapter_10.Section_3.DataStore
import Chapter_10.Section_3.Shape_abs

-- Exercise 1
getValues :
    DataStore (SString .+. val_schema) ->
    List (SchemaType val_schema)
getValues input with (storeView input)
    getValues input | SNil = []
    getValues (addToStore (_, value) store) | SAdd (_, value) store rec =
        value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore =
    addToStore ("First", 1) $
    addToStore ("Second", 2) $
    empty

-- Exercise 2
-- See Chapter_10.Section_3.Shape_abs

area : Shape -> Double
area s with (shapeView s)
    area (triangle base height) | STriangle = base * height / 2
    area (rectangle width height) | SRectangle = width * height
    area (circle radius) | SCircle = pi * radius * radius