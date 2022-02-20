module Chapter_5.Section_2.Exercises

import Chapter_5.Section_2.ReadNum

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
