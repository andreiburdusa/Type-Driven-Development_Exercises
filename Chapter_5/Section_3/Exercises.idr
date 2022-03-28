module Chapter_5.Section_3.Exercises

import System.File
import Data.String
import Data.Vect

-- Exercise 1
readToBlank : IO (List String)
readToBlank = do
    str <- getLine
    if str == ""
        then pure []
        else do
            strs <- readToBlank
            pure $ str :: strs

-- Exercise 2
readAndSave : IO ()
readAndSave = do
    putStrLn "Enter input (empty line to end):"
    input <- readToBlank
    putStr "Enter filename: "
    fileName <- getLine
    Right _ <- writeFile fileName (unlines input)
        | Left err => printLn err
    pure ()

-- Exercise 3
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right file <- openFile filename Read
        | Left err => pure (_ ** [])
    Just depPair <- loop file
        | Nothing => pure (_ ** [])
    closeFile file
    pure depPair
  where
    loop : File -> IO (Maybe (DPair Nat (\n => Vect n String)))
    loop file = do
        False <- fEOF file
            | True => (pure . pure) (_ ** [])
        Right line <- fGetLine file
            | Left _ => pure Nothing
        Just (_ ** lines) <- loop file
            | Nothing => pure Nothing
        -- ?rhs
        pure . pure $ (_ ** line :: lines)
