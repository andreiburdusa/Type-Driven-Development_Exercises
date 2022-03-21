module Chapter_12.Section_3.ArithState

import Data.String
import Data.Primitives.Views
import Data.Stream
import Data.Bits
import System

-- %default total

record Score where
    constructor MkScore
    correct : Nat
    attempted : Nat

record GameState where
    constructor MkGameState
    score : Score
    difficulty : Int

Show GameState where
    show st =
        show (correct (score st))
        ++ "/"
        ++ show (attempted (score st))
        ++ "\n"
        ++ "Difficulty: "
        ++ show (difficulty st)

setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff {- state -} = { difficulty := newDiff } {- state -}

addWrong : GameState -> GameState
addWrong state =
    { score->attempted $= (+1) } state

addCorrect : GameState -> GameState
addCorrect state =
    { score->attempted $= (+1)
    , score->correct $= (+1)
    }
    state

initState : GameState
initState = MkGameState (MkScore 0 0) 12

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String

    GetRandom : Command Int
    GetGameState : Command GameState
    PutGameState : GameState -> Command ()

    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    Seq : Command () -> Inf (ConsoleIO a) -> ConsoleIO a

{-
namespace CommandDo
  export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

  export
  (>>) : Command () -> Command b -> Command b
  ma >> mb = Bind ma (\ _ => mb)
-}

namespace ConsoleDo
  export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

  export
  (>>) : Command () -> Inf (ConsoleIO b) -> ConsoleIO b
  (>>) = Seq

{-
"As an exercise, try providing implementations of each. As I
noted in the last chapter, however, you can’t provide a Monad implementa-
tion for ConsoleIO because the type of ConsoleDo.(>>=) doesn’t fit."
-}
-- Also, Exercise 2
mutual
    Functor Command where
        map func x = do
            val <- x
            pure (func val)
    Applicative Command where
        pure = Pure
        (<*>) f a = do
            f' <- f
            a' <- a
            pure (f' a')
    Monad Command where
        (>>=) = Bind

-- Exercise 1
updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
    state <- GetGameState
    PutGameState (f state)

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

-- runCommand : Command a -> IO a
-- runCommand (PutStr x) = putStr x
-- runCommand GetLine = getLine
-- runCommand (Pure val) = pure val
-- runCommand (Bind c f) = do
--     res <- runCommand c
--     runCommand (f res)
-- runCommand (PutGameState _) = ?runCommand_missing_case_3
-- runCommand GetGameState = ?runCommand_missing_case_2
-- runCommand GetRandom = ?runCommand_missing_case_1

runCommand :
    Stream Int ->
    GameState ->
    Command a ->
    IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do
    putStr x
    pure ((), rnds, state)
runCommand rnds state GetLine = do
    str <- getLine
    pure (str, rnds, state)
runCommand (val :: rnds) state GetRandom =
    pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
        getRandom val 0 | DivByZero = 1
        getRandom ((max * div) + rem) max | (DivBy div rem prf) = abs rem + 1
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) = do
    (res, newRnds, newState) <- runCommand rnds state c
    runCommand newRnds newState (f res)


-- run : Fuel -> ConsoleIO a -> IO (Maybe a)
-- run fuel (Quit val) = pure (Just val)
-- run (More fuel) (Do c f) = do
--     res <- runCommand c
--     run fuel (f res)
-- run (More fuel) (Seq c d) = do
--     runCommand c
--     run fuel d
-- run Dry p = pure Nothing

run :
    Fuel ->
    Stream Int ->
    GameState ->
    ConsoleIO a ->
    IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f) = do
    (res, newRnds, newState) <- runCommand rnds state c
    run fuel newRnds newState (f res)
run Dry rnds state p = pure (Nothing, rnds, state)
run (More fuel) rnds state (Seq c f) = do
    ((), newRnds, newState) <- runCommand rnds state c
    run fuel newRnds newState f
run (More _) _ _ (Seq _ _) = ?run_missing_case_1

mutual
    correct : ConsoleIO GameState
    correct = do
        PutStr "Correct!\n"
        updateGameState addCorrect
        -- st <- GetGameState
        -- PutGameState (addCorrect st)
        quiz

    wrong : Int -> ConsoleIO GameState
    wrong ans = do
        PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
        updateGameState addWrong
        -- st <- GetGameState
        -- PutGameState (addWrong st)
        quiz

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

    quiz : ConsoleIO GameState
    quiz = do
        num1 <- GetRandom
        num2 <- GetRandom
        st <- GetGameState
        PutStr (show st ++ "\n")

        input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
        case input of
            Answer answer =>
                if answer == num1 * num2 then correct else wrong (num1 * num2)
            QuitCmd =>
                Quit st

randoms : Int -> Stream Int
randoms seed =
    let seed' = 1664525 * seed + 1013904223 in
        (seed' `shiftR` 2) :: randoms seed'

partial
main : IO ()
main = do
    seed <- time
    (Just score, _, state) <-
        run forever (randoms (fromInteger seed)) initState quiz
    | _ => putStrLn "Ran out of fuel"
    putStrLn ("Final score: " ++ show state)