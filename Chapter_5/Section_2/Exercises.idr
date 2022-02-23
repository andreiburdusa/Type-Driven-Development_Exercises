module Chapter_5.Section_2.Exercises

import Chapter_5.Section_2.ReadNum
import Chapter_4.SumInputs -- for testing myReplWith
import System

-- Exercise 1
mutual
  tooLow : Nat -> IO ()
  tooLow target = do
    putStrLn "Too low!"
    guess target

  correct : Nat -> IO ()
  correct target = do
    putStrLn "Correct!"
  
  tooHigh : Nat -> IO ()
  tooHigh target = do
    putStrLn "Too high"
    guess target

  guess : (target : Nat) -> IO ()
  guess target = do
    putStr "Enter your guess: "
    Just n <- readNumber
      | Nothing => do
        putStrLn "Invalid input!"
        guess target
    case compare n target of
      LT => tooLow target
      EQ => correct target
      GT => tooHigh target

-- Exercise 2

main : IO ()
main = do
  seconds <- time
  let random = seconds `mod` 100 + 1
  guess (cast random)

-- Exercise 3
guess' : (target : Nat) -> (guesses : Nat) -> IO ()
guess' target guesses = do
  putStr $ "Enter your guess (" ++ show guesses ++ " so far): "
  Just n <- readNumber
    | Nothing => do
      putStrLn "Invalid input!"
      guess' target guesses
  case compare n target of
    LT => tooLow' target
    EQ => correct' target
    GT => tooHigh' target
  where

    tooLow' : Nat -> IO ()
    tooLow' target = do
      putStrLn "Too low!"
      guess' target (guesses + 1)

    correct' : Nat -> IO ()
    correct' target = do
      putStrLn "Correct!"
    
    tooHigh' : Nat -> IO ()
    tooHigh' target = do
      putStrLn "Too high"
      guess' target (guesses + 1)

-- Exercise 4
myRepl : String -> (String -> String) -> IO ()
myRepl initialMessage f = do
  putStr initialMessage
  input <- getLine
  putStr (f input)
  myRepl initialMessage f

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state initialMessage f = do
  putStr initialMessage
  input <- getLine
  let Just (output, newState) = f state input
    | Nothing => pure ()
  putStr output
  myReplWith newState initialMessage f
  