module Chapter_9.Section_1.RemoveElem

import Data.Vect.Elem
import Data.Vect

export
removeElem :
    {n:_} ->
    (value : a) ->
    (xs : Vect (S n) a) ->
    (prf : Elem value xs) ->
    Vect n a
removeElem value (value :: xs) Here = xs
removeElem {n = 0} value (y :: []) (There later) = absurd later
-- also typechecks:
-- removeElem {n = 0} value (y :: []) (There later) = []
removeElem {n = S k} value (y :: xs) (There later) = y :: removeElem value xs later

export
removeElem_auto :
    {n:_} ->
    (value : a) ->
    (xs : Vect (S n) a) ->
    {auto prf : Elem value xs} ->
    Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

removeElem' :
    {n:_} ->
    (value : a) ->
    (xs : Vect (S n) a) ->
    {auto prf : Elem value xs} ->
    Vect n a
removeElem' value (value :: xs) {prf = Here} =
    xs
removeElem' {n = 0} value (y :: []) {prf = (There later)} =
    absurd later
removeElem' {n = (S k)} value (y :: xs) {prf = (There later)} =
    y :: removeElem' value xs