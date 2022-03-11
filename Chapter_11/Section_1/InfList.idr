module Chapter_11.Section_1.InfList

import Data.Stream

public export
data InfList : Type -> Type where
    (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, yx, zs

export
countFrom : Integer -> InfList Integer
countFrom x = x :: {- Delay ( -} countFrom (x + 1) {- ) -}

export
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k {- (Force -} xs {- ) -}
