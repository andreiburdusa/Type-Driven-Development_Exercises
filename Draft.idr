import Data.Vect

createEmpties : {n : _} -> Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper :
    (x : Vect n elem) ->
    (xsTrans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : {n:_} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
    let xsTrans = transposeMat xs in
    -- transposeHelper x xsTrans
    zipWith (::) x xsTrans

addMatrix :
    Num a =>
    Vect n (Vect m a) ->
    Vect n (Vect m a) ->
    Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

zipRow : Num a => Vect m a -> Vect m a -> a
zipRow xs ys = sum (zipWith (*) xs ys)

firstRow :
    Num a =>
    (x : Vect m a) ->
    (transposedRight : Vect p (Vect m a)) ->
    Vect p a
firstRow x transposedRight = map (zipRow x) transposedRight

multWorker :
    Num a =>
    (ls : Vect n (Vect m a)) ->
    (transposedRight : Vect p (Vect m a)) ->
    Vect n (Vect p a)
multWorker [] transposedRight = []
multWorker (x :: xs) transposedRight =
    firstRow x transposedRight :: multWorker xs transposedRight

multMatrix :
    Num a =>
    {p:_} ->
    Vect n (Vect m a) ->
    Vect m (Vect p a) ->
    Vect n (Vect p a)
multMatrix ls rs =
    let transposedRight = transposeMat rs
    in (multWorker ls transposedRight)

createEmpties' : {n:_} -> Vect n (Vect 0 elem)
createEmpties' {n = Z} = []
createEmpties' {n = (S k)} = [] :: createEmpties'
