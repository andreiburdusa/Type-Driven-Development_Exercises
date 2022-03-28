module Chapter_3.Section_3.Exercises

import Data.Vect
import Chapter_3.Section_3.Matrix

-- Exercise 1
transposeMat : {n:_} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
    let xsTrans = transposeMat xs in
    zipWith (::) x xsTrans

-- Exercise 2
addMatrix :
    Num a =>
    Vect n (Vect m a) ->
    Vect n (Vect m a) ->
    Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

-- Exercise 3

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
