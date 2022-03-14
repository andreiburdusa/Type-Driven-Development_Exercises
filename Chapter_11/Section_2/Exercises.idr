module Chapter_11.Section_2.Exercises

import Chapter_11.Section_2.ArithTotal

import Data.String

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
    putStr prompt
    input <- getLine
    putStr (action input)
    totalREPL prompt action
