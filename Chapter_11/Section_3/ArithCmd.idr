module Chapter_11.Section_3.ArithCmd

import Data.Primitives.Views
import Data.Bits
import Data.String
import System

import System.File.ReadWrite

%default total
{-
See:
https://github.com/idris-lang/Idris2/blob/main/tests/typedd-book/chapter11/ArithCmd.idr
-}

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String
	-- Exercise 2
    ReadFile : String -> Command (Either FileError String)
    WriteFile : String -> String -> Command (Either FileError ())

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

%tcinline
(>>) : Command () -> Inf (ConsoleIO b) -> ConsoleIO b
ma >> mb = Do ma (\ _ => mb)

partial
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
-- Exercise 2
-- readFile is partial, so runCommand is partial
runCommand (ReadFile fileName) = readFile fileName
runCommand (WriteFile fileName contents) = writeFile fileName contents

data Fuel' = Dry | More (Lazy Fuel')

partial
forever : Fuel'
forever = More forever

partial
run : Fuel' -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = pure (Just val)
run (More fuel) (Do c f) = do
    res <- runCommand c
    run fuel (f res)
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

mutual
    correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
    correct nums score = do
        PutStr "Correct!\n"
        quiz nums (score + 1)
    wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
    wrong nums ans score = do
        PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
        quiz nums score
    quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
    quiz (num1 :: num2 :: nums) score = do
        PutStr ("Score so far: " ++ show score ++ "\n")
        PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
        answer <- GetLine
        if toLower answer == "quit"
          then
            Quit score
          else
            if (cast answer == num1 * num2)
              then
                correct nums score
              else
                wrong nums (num1 * num2) score

partial
main : IO ()
main = do
    seed <- time
    Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
    | Nothing => putStrLn "Ran out of fuel"
    putStrLn ("Final score: " ++ show score)

-- Exercise 3
mutual
	partial
	copy : (source : String) -> (destination : String) -> ConsoleIO ()
	copy source destination = do
		Right content <- ReadFile source
		| Left err => do
			_ <- PutStr "Error while reading file\n"
			shell
		_ <- WriteFile destination content
		shell

	partial
	cat : String -> ConsoleIO ()
	cat source = do
		Right content <- ReadFile source
		| Left err => do
			_ <- PutStr "Error while reading file\n"
			shell
		_ <- PutStr content
		shell

	partial
	shell : ConsoleIO ()
	shell = do
		PutStr "$> " 
		command <- GetLine
		case words command of
			["exit"] => Quit ()
			["copy", source, destination] => copy source destination
			["cat", filename] => cat filename
			[] => shell
			_ => do
				_ <- PutStr "Error while reading command\n"
				shell

partial
mainShell : IO ()
mainShell = do
	Just () <- run forever shell
	| Nothing => putStrLn "Ran out of fuel"
	pure ()
{-
Chapter_11.Section_3.ArithCmd> :exec mainShell
$> cat savedOutput
abc
$> copy savedOutput savedOutputCopy
$> cat savedOutputCopy
abc
$> 
$> nonExistentCommand
Error while reading command
$> cat nonExistentFile
Error while reading file
$> copy nonExistentFile destination
Error while reading file
$> 
$> 
$> exit
Chapter_11.Section_3.ArithCmd> 
-}