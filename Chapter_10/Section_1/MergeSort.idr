module Chapter_10.Section_1.MergeSort

import Data.List

data SplitList : List a -> Type where
    SplitNil : SplitList []
    SplitOne : SplitList [x]
    SplitPair :
        (lefts : List a) -> (rigths : List a) -> SplitList (lefts ++ rigths)

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input
  where
    splitListHelp : List a -> (input : List a) -> SplitList input
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: counter) (item :: items) =
        case splitListHelp counter items of
            SplitNil => SplitOne
            SplitOne {x} => SplitPair [item] [x]
            SplitPair lefts rights => SplitPair (item :: lefts) rights
    splitListHelp _ items = SplitPair [] items

mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
    mergeSort [] | SplitNil = []
    mergeSort [x] | SplitOne = [x]
    mergeSort (lefts ++ rigths) | (SplitPair lefts rigths) =
        merge (mergeSort lefts) (mergeSort rigths)
