module Chapter_14.Section_3.Hangman

import Data.Vect.Elem -- `Elem` now has its own submodule
import Data.String -- for `toUpper`
import Data.List -- for `nub`
import Data.Vect

import Decidable.Equality

%default total

data GameState : Type where
    NotRunning : GameState
    Running : (guesses : Nat) -> (letters : Nat) -> GameState

letters : String -> List Char
letters = nub . map toUpper . unpack

data GuessResult = Correct | Incorrect

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
    NewGame :
        (word : String) ->
        GameCmd
            ()
            NotRunning
            (const $ Running 6 (length (letters word)))
    Won :
        GameCmd
            ()
            (Running (S guesses) 0)
            (const NotRunning)
    Lost :
        GameCmd
            ()
            (Running 0 (S guesses))
            (const NotRunning)
    Guess :
        (c : Char) ->
        GameCmd
            GuessResult
            (Running (S guesses) (S letters))
            (\res =>
                case res of
                    Correct => Running (S guesses) letters
                    Incorrect => Running guesses (S letters)
            )
    ShowState : GameCmd () state (const state)
    Message   : String -> GameCmd () state (const state)
    ReadGuess : GameCmd Char state (const state)

    Pure  : (res : ty) -> GameCmd ty (state_fn res) state_fn
    (>>=) :
        GameCmd a state1 state2_fn ->
        ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
        GameCmd b state1 state3_fn

(>>) : GameCmd () state1 state2_fn ->
       Lazy (GameCmd a (state2_fn ()) state3_fn) ->
       GameCmd a state1 state3_fn
ma >> mb = ma >>= \ () => mb

namespace Loop
    public export
    data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
        (>>=) :
            GameCmd a state1 state2_fn ->
            ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
            GameLoop b state1 state3_fn
        (>>) : GameCmd () state1 state2_fn ->
           Inf (GameLoop a (state2_fn ()) state3_fn) ->
           GameLoop a state1 state3_fn
        Exit : GameLoop () NotRunning (const NotRunning)

gameLoop :
    {guesses:_} ->
    {letters:_} ->
    GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameLoop {guesses} {letters} = do
    ShowState
    g <- ReadGuess
    ok <- Guess g
    case ok of
        Correct =>
            case letters of
                0 => do
                    Won
                    ShowState
                    Exit
                S k => do Message "Correct"; gameLoop;
        Incorrect =>
            case guesses of
                0 => do
                    Lost
                    ShowState
                    Exit
                (S k) => do
                    Message "Incorrect"
                    gameLoop

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do
    NewGame "testing"
    gameLoop

data Game : GameState -> Type where
    GameStart  : Game NotRunning
    GameWon    : (word : String) -> Game NotRunning
    GameLost   : (word : String) -> Game NotRunning
    InProgress :
        {letters:_} ->
        (word : String) ->
        (guesses : Nat) ->
        (missing : Vect letters Char) ->
        Game (Running guesses letters)

Show (Game g) where
    show GameStart = "Starting"
    show (GameWon word) = "Game won: word was " ++ word
    show (GameLost word) = "Game lost: word was " ++ word
    show (InProgress word guesses missing) =
        "\n"
        ++ pack (map hideMissing (unpack word))
        ++ "\n"
        ++ show guesses
        ++ " guesses left "
      where
        hideMissing : Char -> Char
        hideMissing c = if c `elem` missing then '-' else c

data Fuel = Dry | More (Lazy Fuel)

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
    OK :
        (res : ty) -> 
        Game (outstate_fn res) ->
        GameResult ty outstate_fn
    OutOfFuel :
        GameResult ty outstate_fn

ok :
    (res : ty) ->
    Game (outstate_fn res) ->
    IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

removeElem :
    {n:_} ->
    (value : a) ->
    (xs : Vect (S n) a) ->
    {auto prf : Elem value xs} ->
    Vect n a
removeElem value (value :: xs) {prf = Here} = xs
removeElem {n = 0} value (y :: []) {prf = (There later)} = absurd later
removeElem {n = (S k)} value (y :: xs) {prf = (There later)} =
    y :: removeElem value xs

runCmd :
    Fuel ->
    Game instate ->
    GameCmd ty instate outstate_fn ->
    IO (GameResult ty outstate_fn)
runCmd fuel state (NewGame word) =
    ok () (InProgress (toUpper word) _ (fromList (letters word)))
runCmd fuel (InProgress word _ missing) Won = ok () (GameWon word)
runCmd fuel (InProgress word _ missing) Lost = ok () (GameLost word)
runCmd fuel (InProgress word _ missing) (Guess c) =
    case isElem c missing of
        Yes prf => ok Correct (InProgress word _ (removeElem c missing))
        No contra => ok Incorrect (InProgress word _ missing)
runCmd fuel state ShowState = do
    printLn state
    ok () state
runCmd fuel state (Message str) = do
    putStrLn str
    ok () state
runCmd (More fuel) state ReadGuess = do
    putStr "Guess: "
    input <- getLine
    case unpack input of
        [x] =>
            if isAlpha x then
                ok (toUpper x) state
            else do
                putStrLn "Invalid input"
                runCmd fuel state ReadGuess
        _ => do
            putStrLn "Invalid input"
            runCmd fuel state ReadGuess
runCmd Dry _ _ = pure OutOfFuel
runCmd fuel state (Pure res) = ok res state
runCmd fuel state (cmd >>= next) = do
    OK cmdRes newSt <- runCmd fuel state cmd
    | OutOfFuel => pure OutOfFuel
    runCmd fuel newSt (next cmdRes)

run :
    Fuel ->
    Game instate ->
    GameLoop ty instate outstate_fn ->
    IO (GameResult ty outstate_fn)
run Dry _ _ = pure OutOfFuel
run (More fuel) state (cmd >>= next) = do
    OK cmdRes newSt <- runCmd fuel state cmd
    | OutOfFuel => pure OutOfFuel
    run fuel newSt (next cmdRes)
run (More fuel) state (cmd >> next) = do
    OK () newSt <- runCmd fuel state cmd
    | OutOfFuel => pure OutOfFuel
    run fuel newSt next
run (More fuel) state Exit = ok () state

%default partial

forever : Fuel
forever = More forever

main : IO ()
main = do
    _ <- run forever GameStart hangman
    pure ()