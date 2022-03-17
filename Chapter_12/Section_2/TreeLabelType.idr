module Chapter_12.Section_2.TreeLabelType

data Tree a = Empty
            | Node (Tree a) a (Tree a)

public export
data State : (stateType : Type) -> Type -> Type where
    Get : State stateType stateType
    Put : stateType -> State stateType ()

    Pure : ty -> State stateType ty
    Bind : State stateType a -> (a -> State stateType b) -> State stateType b

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

pure : ty -> State stateType ty
pure = Pure

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right) = do
    left_labelled <- treeLabelWith left
    (this :: rest) <- get
    _ <- put rest
    right_labelled <- treeLabelWith right
    pure (Node left_labelled (this, val) right_labelled)

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st =
    let (val, nextState) = runState cmd st in
        runState (prog val) nextState

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = fst (runState (treeLabelWith tree) [1..])