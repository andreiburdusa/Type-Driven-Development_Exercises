module Chapter_11.Section_3.Greet

import Chapter_11.Section_2.ArithTotal

greet : InfIO
greet = do
    putStr "Enter your name: "
    name <- getLine
    putStrLn ("Hello " ++ name)
    greet
