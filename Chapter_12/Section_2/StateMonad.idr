module Chapter_12.Section_2.StateMonad

import Chapter_12.Section_2.TreeLabelType

{-
Functor (State stateType) where
    map func x = Bind x (\val => Pure (func val))

Applicative (State stateType) where
    pure = Pure
    (<*>) f a =
        Bind f (\f' =>
        Bind a (\a' =>
        Pure (f' a')))

Monad (State stateType) where
    (>>=) = Bind
-}

mutual
    Functor (State stateType) where
        map func x = do
            val <- x
            pure (func val)
    Applicative (State stateType) where
        pure = Pure
        (<*>) f a = do
            f' <- f
            a' <- a
            pure (f' a')
    Monad (State stateType) where
            (>>=) = Bind