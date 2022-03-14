module Chapter_11.Section_2.InfIO

data InfIO : Type where
    Do : IO a -> (a -> Inf InfIO) -> InfIO

loopPrint : String -> InfIO
loopPrint msg =
    Do (putStrLn msg) (\_ => loopPrint msg)

run0 : InfIO -> IO ()
run0 (Do action cont) = do
    res <- action
    run0 (cont res)

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do
    res <- c
    run fuel (f res)
run Dry p = putStrLn "Out of fuel"