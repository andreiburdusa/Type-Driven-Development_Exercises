import Chapter_5.Section_2.ReadNum

guess : (target : Nat) -> IO ()
guess target = do
  putStr "Enter your guess: "
  Just n <- readNumber
  ?rhs
