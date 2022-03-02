module Chapter_8.Section_3.CheckEqDec

zeroNotSucc : 0 = S k -> Void
zeroNotSucc Refl impossible

sucNotZer : S k = 0 -> Void
sucNotZer Refl impossible

noRec : (k = j -> Void) -> S k = S j -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat 0 0 = Yes Refl
checkEqNat 0 (S k) = No zeroNotSucc
checkEqNat (S k) 0 = No sucNotZer
checkEqNat (S k) (S j) =
    case checkEqNat k j of
        Yes prf => Yes (cong S prf)
        No contra => No (noRec contra)
