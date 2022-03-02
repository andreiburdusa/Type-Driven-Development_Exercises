module Chapter_8.Section_2.ReverseVec

import Data.Vect

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse {n = S k} (x :: xs) =
    reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k' + 1) a -> Vect (S k') a
    reverseProof {k'} result = rewrite plusCommutative 1 k' in result

 