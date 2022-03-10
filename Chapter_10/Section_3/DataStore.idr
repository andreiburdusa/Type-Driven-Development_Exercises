module Chapter_10.Section_3.DataStore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString | SInt | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

export
record DataStore (schema : Schema) where
    constructor MkData
    size : Nat
    items : Vect size (SchemaType schema)

export
empty : DataStore schema
empty = MkData _ []

export
addToStore :
    (value : SchemaType schema) ->
    (store : DataStore schema) ->
    DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

public export
data StoreView : (schema : _) -> DataStore schema -> Type where
    SNil : StoreView schema empty
    SAdd :
        (value, store : _) ->
        (rec : StoreView schema store) ->
        StoreView schema (addToStore value store)

storeViewHelp :
    {size : _} -> 
    (items : Vect size (SchemaType schema)) ->
    StoreView schema (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (x :: xs) = SAdd _ _ (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView schema store
storeView (MkData _ items) = storeViewHelp items