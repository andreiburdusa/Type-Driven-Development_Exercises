module Chapter_11.Section_3.RunIO

{-
See:
https://github.com/idris-lang/Idris2/blob/main/tests/typedd-book/chapter11/RunIO.idr
-}

data RunIO : Type -> Type where
    Quit : a -> RunIO  a
    Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
    Seq : IO () -> Inf (RunIO b) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) :  IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

greet : RunIO ()
greet = do
    putStr "Enter your name: "
    name <- getLine
    if name == ""
      then do
        putStrLn "Bye bye!"
        Quit ()
      else do
        putStrLn ("Hello " ++ name)
        greet

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do
    res <- c
    run fuel (f res)
run (More fuel) (Seq c d) = do
    c
    run fuel d
run Dry p = pure Nothing


partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do
    _ <- run forever greet
    pure ()