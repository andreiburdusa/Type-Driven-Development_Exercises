module Chapter_13.Section_1.Exercise_2

-- Exercise 2
data GuessCmd : Type -> Nat -> Nat -> Type where
    Try : Integer -> GuessCmd Ordering (S guesses) guesses
    Pure : ty -> GuessCmd ty state state
    (>>=) :
        GuessCmd a state1 state2 ->
        (a -> GuessCmd b state2 state3) ->
        GuessCmd b state1 state3
(>>) :
    GuessCmd _ state1 state2 ->
    Lazy (GuessCmd a state2 state3) ->
    GuessCmd a state1 state3
ma >> mb = ma >>= \ _ => mb

threeGuesses: GuessCmd () 3 0
threeGuesses = do
    Try 10
    Try 20
    Try 15
    Pure ()

-- Doesn't typecheck
{-
noGuesses : GuessCmd () 0 0
noGuesses = do
    Try 10
    Pure ()
-}