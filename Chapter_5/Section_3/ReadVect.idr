module Chapter_5.Section_3.ReadVect

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen 0 = pure []
readVectLen (S k) = do
    x <- getLine
    xs <- readVectLen k
    pure (x :: xs)

data VectUnkown : Type -> Type where
    MkVect : (len : Nat) -> Vect len a -> VectUnkown a

readVect : IO (VectUnkown String)
readVect = do
    x <- getLine
    if x == ""
        then
            pure (MkVect _ [])
        else do
            MkVect _ xs <- readVect
            pure $ MkVect _ (x :: xs)

printVect : Show a => VectUnkown a -> IO ()
printVect (MkVect len xs) =
    putStrLn (show xs ++ " (length " ++ show len ++ ")")

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])
