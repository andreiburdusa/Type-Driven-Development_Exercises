module Main
main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

printLength : IO ()
printLength =
  getLine >>= \input => let len = length input in putStrLn (show len)

printLonger : IO ()
printLonger = do
  putStr "First string: "
  firstString <- getLine
  putStr "Second string: "
  secondString <- getLine
  let length1 = Prelude.Types.String.length firstString
  let length2 = Prelude.Types.String.length secondString
  putStrLn $ show (max length1 length2)

printLonger' : IO ()
printLonger' = do
  putStr "First string: " >>= \_ =>
    getLine >>= \firstString =>
    putStr "Second string: " >>= \_ =>
    getLine >>= \secondString =>
      let length1 = Prelude.Types.String.length firstString
          length2 = Prelude.Types.String.length secondString in
      putStrLn $ show (max length1 length2)
