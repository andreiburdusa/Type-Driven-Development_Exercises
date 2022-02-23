module Chapter_6.Section_2.Adder

AdderType : (numargs : Nat) -> Type
AdderType 0 = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder 0 acc = acc
adder (S k) acc = \next => adder k (next + acc)


