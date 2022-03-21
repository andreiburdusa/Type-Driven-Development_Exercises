module Chapter_13.Section_1.Exercise_3

-- Exercise 3

data Matter = Solid | Liquid | Gas

data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid

    Pure : ty -> MatterCmd ty matter matter
    Bind :
        MatterCmd a matter1 matter2 ->
        (a -> MatterCmd b matter2 matter3) ->
        MatterCmd b matter1 matter3

(>>=) :
    MatterCmd a matter1 matter2 ->
    (a -> MatterCmd b matter2 matter3) ->
    MatterCmd b matter1 matter3
(>>=) = Bind

(>>) :
    MatterCmd _ matter1 matter2 ->
    MatterCmd a matter2 matter3 ->
    MatterCmd a matter1 matter3
ma >> mb = Bind ma (\ _ => mb)

iceSteam : MatterCmd () Solid Gas
iceSteam = do
    Melt
    Boil

steamIce : MatterCmd () Gas Solid
steamIce = do
    Condense
    Freeze

-- Doesn't typecheck
{-
overMelt : MatterCmd () Solid Gas
overMelt = do
    Melt
    Melt
-}