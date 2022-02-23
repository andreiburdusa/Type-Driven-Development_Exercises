module Main

import Data.Vect
import System.REPL
import Data.String

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

data Command = Add String | Get Integer | Quit | Size | Search String

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (i :: is) = i :: addToData is

getEntry :
  (pos : Integer) ->
  (store : DataStore) ->
  Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store
  in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id store_items ++ "\n", store)

indices : (p : Nat) -> Vect p Nat
indices Z = []
indices (S n) = (n) :: indices n

searched : (store : DataStore) -> (substring : String) -> String
searched (MkData size items) substring =
  let indexed = zip (reverse $ indices size) items
      (p ** found) = Data.Vect.filter ((substring `isInfixOf`) . snd) indexed
  in
    concat . map (++ "\n") . map (\(index, name) => show index ++ ": " ++ name) $
      found


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) =>
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Search substring) =>
      Just (searched store substring, store)
    Just (Get pos) => getEntry pos store
    Just Size =>
      Just (show (size store) ++ "\n", store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
