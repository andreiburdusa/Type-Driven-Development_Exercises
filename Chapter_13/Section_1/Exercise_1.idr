module Chapter_13.Section_1.Exercise_1

-- Exercise 1
data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type -> DoorState -> DoorState -> Type where
    Open     : DoorCmd () DoorClosed DoorOpen
    Close    : DoorCmd () DoorOpen DoorClosed
    RingBell : DoorCmd () doorState doorState

    Pure : ty  -> DoorCmd a state state
    (>>=) :
        DoorCmd a state1 state2 ->
        (a -> DoorCmd b state2 state3)
        -> DoorCmd b state1 state3

(>>) :
    DoorCmd () state1 state2 ->
    Lazy (DoorCmd a state2 state3) ->
    DoorCmd a state1 state3
ma >> mb = ma >>= \ _ => mb

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do
    RingBell
    Open
    RingBell
    Close