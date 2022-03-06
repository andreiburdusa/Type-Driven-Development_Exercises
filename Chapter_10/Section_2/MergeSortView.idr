module Chapter_10.Section_2.MergeSortView

import Data.List.Views
import Data.List

mergeSort : Ord a => List a -> List a
mergeSort input with (splitRec input)
    mergeSort [] | SplitRecNil = []
    mergeSort [x] | (SplitRecOne x) = [x]
    mergeSort (lefts ++ rights) | (SplitRecPair lefts rights lrec rrec) =
        merge (mergeSort lefts | lrec) (mergeSort rights | rrec)
