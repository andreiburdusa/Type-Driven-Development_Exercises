module Chapter_11.Section_3.ArithCmdDo

import Data.String
import System
import Data.Primitives.Views
import Data.Bits

{-
See:
https://github.com/idris-lang/Idris2/blob/main/tests/typedd-book/chapter11/ArithCmdDo.idr
-}

%default total

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String

    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    Seq : Command () -> Inf (ConsoleIO a) -> ConsoleIO a

namespace CommandDo
  export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

  export
  (>>) : Command () -> Command b -> Command b
  ma >> mb = Bind ma (\ _ => mb)

namespace ConsoleDo
  export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

  export
  (>>) : Command () -> Inf (ConsoleIO b) -> ConsoleIO b
  (>>) = Seq

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do
    res <- runCommand c
    runCommand (f res)

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do
    PutStr prompt
    answer <- GetLine
    if toLower answer == "quit"
      then
        Pure QuitCmd
      else
        Pure (Answer (cast answer))

record Score where
    constructor MkScore
    correctAnswers : Nat
    totalAnswers : Nat

mutual
    correct : Stream Int -> (score : Score) -> ConsoleIO Score
    correct nums (MkScore correctAnswers totalAnswers) = do
        PutStr "Correct!\n"
        quiz nums (MkScore (correctAnswers + 1) (totalAnswers + 1))
    wrong : Stream Int -> Int -> (score : Score) -> ConsoleIO Score
    wrong nums ans (MkScore correctAnswers totalAnswers) = do
        PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
        quiz nums (MkScore correctAnswers (totalAnswers + 1))
    quiz : Stream Int -> (score : Score) -> ConsoleIO Score
    quiz (num1 :: num2 :: nums) score@(MkScore correctAnswers totalAnswers) = do
        PutStr
            ( "Score so far: "
            ++ show  correctAnswers
            ++ "/"
            ++ show totalAnswers
            ++ "\n"
            )
        input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
        case input of
            Answer answer =>
                if answer == num1 * num2
                then
                    correct nums score
                else
                    wrong nums (num1 * num2) score
            QuitCmd => Quit score

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = pure (Just val)
run (More fuel) (Do c f) = do
    res <- runCommand c
    run fuel (f res)
run (More fuel) (Seq c d) = do
    runCommand c
    run fuel d
run Dry p = pure Nothing

randoms : Int -> Stream Int
randoms seed =
    let seed' = 1664525 * seed + 1013904223
      in
        (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy div rem prf) = abs rem + 1

partial
main : IO ()
main = do
    seed <- time
    Just (MkScore correctAnswers totalAnswers) <-
        run forever (quiz (arithInputs (fromInteger seed)) (MkScore 0 0))
    | Nothing => putStrLn "Ran out of fuel"
    putStrLn
        ( "Final score: "
        ++ show  correctAnswers
        ++ "/"
        ++ show totalAnswers
        ++ "\n"
        )