module Chapter_13.Section_2.StackIO

import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
    Push : Integer -> StackCmd () height (S height)
    Pop : StackCmd Integer (S height) height
    Top : StackCmd Integer (S height) (S height)

    GetStr : StackCmd String height height
    PutStr : String -> StackCmd () height height

    Pure : ty -> StackCmd ty height height
    (>>=) :
        StackCmd a height1 height2 ->
        (a -> StackCmd b height2 height3) ->
        StackCmd b height1 height3

(>>) :
    StackCmd a height1 height2 ->
    Lazy (StackCmd b height2 height3) ->
    StackCmd b height1 height3
(>>) x y = x >>= (\_ => y)


runStack :
    (stk : Vect inHeight Integer) ->
    StackCmd ty inHeight outHeight ->
    IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do
    x <- getLine
    pure (x, stk)
runStack stk (PutStr x) = do
    putStr x
    pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do
    (x', newStk) <- runStack stk x
    runStack newStk (f x')

data StackIO : Nat -> Type where
    Do :
        StackCmd a height1 height2 ->
        (a -> Inf (StackIO height2)) ->
        StackIO height1
    Seq :
        StackCmd () height1 height2 ->
        Inf (StackIO height2) ->
        StackIO height1

namespace StackDo
    export
    (>>=) :
        StackCmd a height1 height2 ->
        (a -> Inf (StackIO height2)) ->
        StackIO height1
    (>>=) = Do

    export
    (>>) :
        StackCmd () height1 height2 ->
        Inf (StackIO height2) ->
        StackIO height1
    (>>) = Seq

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do
    (res, newStk) <- runStack stk c
    run fuel newStk (f res)
run (More fuel) stk (Seq c k) = do
    ((), newStk) <- runStack stk c
    run fuel newStk k
run Dry _ _ = pure ()

data StkInput
    = Number Integer
    | Add

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput x =
    if all isDigit (unpack x) then
        Just (Number (cast x))
    else
        Nothing

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
    val1 <- Pop
    val2 <- Pop
    Push (val1 + val2)

mutual
    tryAdd : {height:_} -> StackIO height
    tryAdd {height = (S (S h))} = do
        doAdd
        result <- Top
        PutStr (show result ++ "\n")
        stackCalc
    tryAdd = do
        PutStr "Fewer than two items on the stack\n"
        stackCalc

    stackCalc : {height:_} -> StackIO height
    stackCalc {height} = do
        PutStr "> "
        input <- GetStr
        case strToInput input of
            Nothing => do
                PutStr "Invalid input\n"
                stackCalc
            Just (Number x) => do
                Push x
                stackCalc
            Just Add => tryAdd

main : IO ()
main = run forever [] stackCalc