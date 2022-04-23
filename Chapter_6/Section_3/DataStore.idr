module Chapter_6.Section_3.DataStore

import Data.Vect
import System.REPL
import Data.String
import Data.List

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema 

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = 
  case xs of
    [] => Just SString
    _ => do
      xs_sch <- parseSchema xs
      pure (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
  case xs of
    [] => Just SInt
    _ => do
      xs_sch <- parseSchema xs
      pure (SInt .+. xs_sch)
parseSchema ("Char" :: xs) =
  case xs of
    [] => Just SChar
    _ => do
      xs_sch <- parseSchema xs
      pure (SChar .+. xs_sch)
parseSchema _ = Nothing

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema'
  Add : SchemaType schema' -> Command schema'
  Get : Maybe Integer -> Command schema'
  Quit : Command schema'

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
 where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: xs) =
    case span (/= '"') xs of
      (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
      _ => Nothing
  getQuoted _ = Nothing
parsePrefix SInt input =
  case span isDigit input of
    ("", rest) => Nothing
    (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = worker (unpack input)
 where
  worker : List Char -> Maybe (Char, String)
  worker (c :: rest) = Just (c, ltrim (pack rest))
  worker _ = Nothing
parsePrefix (schemal .+. schemar) input = do
  (l_val, input') <- parsePrefix schemal input
  (r_val, input'') <- parsePrefix schemar input'
  Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  _ => Nothing
parseCommand :
  (schema : Schema) ->
  (cmd : String) ->
  (args : String) ->
  Maybe (Command schema)
parseCommand schema "add" str =
  case parseBySchema schema str of
    Nothing => Nothing
    Just restOk => Just (Add restOk)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val =
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (Just $ cast val))
-- parseCommand schema "search" str = Just (Search str)
-- parseCommand "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do
  schemaOk <- parseSchema (words rest)
  pure (SetSchema schemaOk)
parseCommand _ _ _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema =
  case size store of
    Z => Just (MkData schema _ [])
    S k => Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema' size items) newitem =
  MkData schema' _ (addToData items)
 where
  addToData :
    Vect oldSize (SchemaType schema') ->
    Vect (S oldSize) (SchemaType schema')
  addToData [] = [newitem]
  addToData (i :: is) = i :: addToData is

display : { schema : _ } -> SchemaType schema -> String
display { schema = SString } item = show item
display { schema = SInt } item = show item
display { schema = SChar} item = show item
display { schema = (x .+. y) } (iteml, itemr) =
  display iteml ++ ", " ++ display itemr

getEntry :
  (pos : Integer) ->
  (store : DataStore) ->
  Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store
  in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (display (index id store_items) ++ "\n", store)

getJust : List (Maybe a) -> List a
getJust [] = []
getJust (Nothing :: xs) = getJust xs
getJust ((Just x) :: xs) = x :: getJust xs

getAll : DataStore -> Maybe (String, DataStore)
getAll store =
  let indices : List Integer
      indices = [0, 1 ..(cast (size store)) -1 ]
      partialResults : List (Maybe (String, DataStore))
      partialResults = map (\pos => getEntry pos store) indices
      strings : List String
      strings = map fst .  getJust $ partialResults
      strings' : List String
      strings' = map (": " ++) strings
      indicesStrings : List String
      indicesStrings = map show [1..(size store)]
      results : List String
      results = zipWith (++) indicesStrings strings'
  in
    pure (concat results, store)

indices : (p : Nat) -> Vect p Nat
indices Z = []
indices (S n) = (n) :: indices n


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) =>
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') =>
      case setSchema store schema' of
        Nothing => Just ("Can't update schema\n", store)
        Just store' => Just ("OK\n", store')
    Just (Get Nothing) => getAll store
    Just (Get (Just pos)) => getEntry pos store
    Just Quit => Nothing

main : IO ()
main =
  replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
