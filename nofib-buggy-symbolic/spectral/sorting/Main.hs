module Main where

import Sort

import G2.Symbolic

main = do
    s <- mkSymbolic
    cs <- getContents
    putStr (mangle s cs)

mangle :: String{-opt-} -> String{-input to sort-} -> String{-output-}
mangle opt inpt
  = (unlines . sort . lines) inpt
  where
    sort = case opt of
         "heapSort"		-> heapSort
         "insertSort"	-> insertSort
         "mergeSort"	-> mergeSort
         "quickSort"	-> quickSort
         "quickSort2"	-> quickSort2
         "quickerSort"	-> quickerSort
         "treeSort"		-> treeSort
         "treeSort2"	-> treeSort2
         _ -> assume (False) (error ("unrecognized opt: "++opt++"\n"))
