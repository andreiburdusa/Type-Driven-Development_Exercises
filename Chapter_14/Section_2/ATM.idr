module Chapter_14.Section_2.ATM

import Data.Vect
import Data.String

PIN : Type
PIN = Vect 4 Char

data ATMState = Ready | CardInserted | Session

data PINCheck = CorrectPIN | IncorrectPIN

data HasCard : ATMState -> Type where
    HasCI      : HasCard CardInserted
    HasSession : HasCard Session

data ATMCmd : (yt : Type) -> ATMState -> (ty -> ATMState) -> Type where
    InsertCard : ATMCmd () Ready (const CardInserted)
    EjectCard  : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
    GetPIN     : ATMCmd PIN CardInserted (const CardInserted)
    CheckPIN   :
        PIN ->
        ATMCmd
            PINCheck 
            CardInserted
            (\check =>
                case check of
                    CorrectPIN => Session
                    IncorrectPIN => CardInserted
            )
    GetAmmount : ATMCmd Nat state (const state)
    Dispense   : (amount : Nat) -> ATMCmd () Session (const Session)
    Message    : String -> ATMCmd () state (const state)  

    Pure  : (res : ty) -> ATMCmd ty (state_fn res) state_fn
    (>>=) :
        ATMCmd a state1 state2_fn ->
        ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
        ATMCmd b state1 state3_fn

(>>) : ATMCmd () state1 state2_fn ->
       Lazy (ATMCmd a (state2_fn ()) state3_fn) ->
      ATMCmd a state1 state3_fn
ma >> mb = ma >>= \ () => mb

atm : ATMCmd () Ready (const Ready)
atm = do
    InsertCard
    pin <- GetPIN
    pinOK <- CheckPIN pin
    Message "Checking Card"
    case pinOK of
        CorrectPIN => do
            cash <- GetAmmount
            Dispense cash
            EjectCard
            Message "Please remove your card and cash"
        IncorrectPIN =>
            EjectCard

testPIN : Vect 4 Char
testPIN = ['1', '2', '3', '4']

readVect : (n : Nat) -> IO (Vect n Char)
readVect 0 = do
    _ <- getLine
    pure []
readVect (S k) = do
    ch <- getChar
    chs <- readVect k
    pure (ch :: chs)

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do
    putStrLn "Please insert your card (press enter)"
    x <- getLine
    pure ()
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = do
    putStr "Enter PIN: "
    readVect 4
runATM (CheckPIN pin) =
    if pin == testPIN then
        pure CorrectPIN
    else
        pure IncorrectPIN
runATM GetAmmount = do
    putStr "How much would you like? "
    x <- getLine
    pure (stringToNatOrZ x)
runATM (Dispense amount) =
    putStrLn ("Here is " ++ show amount)
runATM (Message msg) = putStrLn msg
runATM (Pure res) = pure res
runATM (x >>= f) = do
    x' <- runATM x
    runATM (f x')

insertEject : ATMCmd () Ready (const Ready)
insertEject = do
    InsertCard
    EjectCard

{-
badATM : ATMCmd () Ready (const Ready)
badATM = EjectCard {HasCI}
-}