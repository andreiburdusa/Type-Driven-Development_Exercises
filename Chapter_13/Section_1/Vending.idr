module Chapter_13.Section_1.Vending

import Data.String

VendState : Type
VendState = (Nat, Nat)

data Input = COIN | VEND | CHANGE | REFILL Nat

data MachineCmd : Type -> VendState -> VendState -> Type where
    InsertCoin : MachineCmd () (pounds, chocs) (S pounds, chocs)
    Vend       : MachineCmd () (S pounds, S chocs) (pounds, chocs)
    GetCoins   : MachineCmd () (pounds, chocs) (Z, chocs)
    Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (Z, bars + chocs)

    Display  : String -> MachineCmd () state state
    GetInput : MachineCmd (Maybe Input) state state

    Pure  : ty -> MachineCmd ty state state
    (>>=) :
        {state2 : _} ->
        MachineCmd a state1 state2 ->
        (a -> MachineCmd b state2 state3) ->
        MachineCmd b state1 state3

data MachineIO : VendState -> Type where
    Do :
        {state1 : _} ->
        MachineCmd a state1 state2 ->
        (a -> Inf (MachineIO state2)) ->
        MachineIO state1
    Seq :
        {state1 : _} ->
        MachineCmd () state1 state2 ->
        Inf (MachineIO state2) -> MachineIO state1

namespace MachineDo
    export
    (>>=) :
        {state1 : _} ->
        MachineCmd a state1 state2 ->
        (a -> Inf (MachineIO state2)) ->
        MachineIO state1
    (>>=) = Do

    export
    (>>) : {state1 : _} ->
            MachineCmd () state1 state2 ->
            Inf (MachineIO state2) -> MachineIO state1
    (>>) = Seq

mutual
    vend : {pounds : _} -> {chocs : _} -> MachineIO (pounds, chocs)
    vend {pounds = 0} = do
        Display "Insert a coin"
        machineLoop
    vend {chocs = 0} = do
        Display "Out of stock"
        machineLoop
    vend {pounds = (S k)} {chocs = (S j)} = do
        Vend
        Display "Enjoy!"
        machineLoop

    refill : {pounds : _} -> {chocs : _} -> (num : Nat) -> MachineIO (pounds, chocs)
    refill {pounds = 0} num = do
        Refill num
        machineLoop
    refill _ = do
        Display "Can't refill: Coins in machine"
        machineLoop
    
    machineLoop : {pounds : _} -> {chocs : _} -> MachineIO (pounds, chocs)
    machineLoop = do
        Just x <- GetInput
        | Nothing => do
            Display "Invalid input"
            machineLoop
        case x of
            COIN => do
                InsertCoin
                machineLoop
            VEND => vend
            CHANGE => do
                GetCoins
                Display "Change returned"
                machineLoop
            REFILL num => refill num

