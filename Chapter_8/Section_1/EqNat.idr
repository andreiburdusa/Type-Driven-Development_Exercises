module Chapter_8.Section_1.EqNat

public export
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num

sameAs : (k : Nat) -> (j : Nat) -> EqNat k j -> EqNat (S k) (S j)
sameAs k k (Same k) = Same (S k)

export
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat 0 0 = Just (Same 0)
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) =
    case checkEqNat k j of
        Nothing => Nothing
        (Just eq) => Just (sameAs _ _ eq)



