module Chapter_13.Section_2.Exercises

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
    | Subtract
    | Multiply
    | Negate
    | Discard
    | Duplicate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x =
    if all isDigit (unpack x) then
        Just (Number (cast x))
    else
        Nothing

doBinaryOp : (Integer -> Integer -> Integer) -> StackCmd () (S (S height)) (S height)
doBinaryOp op = do
    val1 <- Pop
    val2 <- Pop
    Push (op val2 val1)

doNegate : StackCmd () (S k) (S k)
doNegate = do
    val <- Pop
    Push (0 - val)

doDiscard : StackCmd Integer (S k) k
doDiscard = do
    top <- Pop
    Pure top

doDuplicate : StackCmd Integer (S k) (S (S k))
doDuplicate = do
    top <- Top
    Push top
    Pure top

mutual
    tryBinaryOp : (Integer -> Integer -> Integer) -> {height:_} -> StackIO height
    tryBinaryOp {height = (S (S h))} op = do
        doBinaryOp op
        result <- Top
        PutStr (show result ++ "\n")
        stackCalc
    tryBinaryOp op = do
        PutStr "Fewer than two items on the stack\n"
        stackCalc

    tryNegate : {height:_} -> StackIO height
    tryNegate {height = (S k)} = do
        doNegate
        result <- Top
        PutStr (show result ++ "\n")
        stackCalc
    tryNegate = do
        PutStr "Fewer than one item on the stack\n"
        stackCalc

    tryDiscard : {height:_} -> StackIO height
    tryDiscard {height = (S k)} = do
        oldTop <- doDiscard
        PutStr $ "Discarded " ++ show oldTop ++ "\n"
        stackCalc
    tryDiscard = do
        PutStr "Fewer than one item on the stack\n"
        stackCalc

    tryDuplicate : {height:_} -> StackIO height
    tryDuplicate {height = (S k)} = do
        duplicated <- doDuplicate
        PutStr $ "Duplicated " ++ show duplicated ++ "\n"
        stackCalc
    tryDuplicate = do
        PutStr "Fewer than one item on the stack\n"
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
            Just Add => tryBinaryOp (+)
            Just Subtract => tryBinaryOp (-)
            Just Multiply => tryBinaryOp (*)
            Just Negate => tryNegate
            Just Discard => tryDiscard
            Just Duplicate => tryDuplicate

main : IO ()
main = run forever [] stackCalc