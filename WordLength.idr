allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

mutual
  isEven : Nat -> Bool
  isEven Z = False
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
