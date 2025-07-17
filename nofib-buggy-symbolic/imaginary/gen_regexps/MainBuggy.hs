---------------------------------------------------------
--       WARNING: THIS PROGRAM CONTAINS A BUG!!!       --
--                                                     --
--  This program belongs to the faulty nofib library   --
--      and contains a bug to benchmark debuggers      --
--                                                     --
---------------------------------------------------------
--                                                     --
--  "The faulty nofib library" is a  collection of     --
--  Haskell programs from the 'nofib' benchmark suite  --
--                                                     --
--  Faults are always marked with a comment: "BUG"     --
--  The commented correct line appears after the       --
--  faulty line marked with "CORRECT"                  --
--                                                     --
--  We welcome any comment or improvement about        --
--  bugs. You can send them to:                        --
--        Josep Silva (jsilva@dsic.upv.es)             --
--                                                     --
---------------------------------------------------------
--                                                     --
--  There are three kinds of bugs depending on their   --
--  consequences:                                      --
--  1) Bugs that produce an incorrect result           --
--  2) Bugs that produce non-termination               --
--  3) Bugs that produce an exception (e.g. div by 0)  --
--                                                     --
--  This program contains a bug of tipe 1              --
---------------------------------------------------------

-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
-- RJE: Modified so it only outputs the number of characters in the output, 
-- rather that the output itself, thus avoiding having to generate such a 
-- huge output file to get a reasonable execution time.

module MainBuggy where

import Data.Char

-- main = interact (("Enter a generator: " ++).show.numchars.expand.head.lines)

numchars :: [String] -> Int
numchars l = sum $ map length l

expand []	= [""]
expand ('<':x)	= numericRule x
expand ('[':x)	= alphabeticRule x
expand x	= constantRule x

constantRule (c:rest) = [ c:z | z <- expand rest ]

alphabeticRule (a:'-':b:']':rest)
  | a <= b  	= [c:z | c <- [a..b],	      z <- expand rest]
  | otherwise	= [c:z | c <- reverse [b..a], z <- expand rest]

numericRule x
  = [ pad (show i) ++ z
   -- BUG: The following line contains a bug:   
	| i <- if u < v then [u..v] else [u,u..v]
   -- CORRECT -- | i <- if u < v then [u..v] else [u,u-1..v] 
	, z <- expand s ]
  where
    (p,_:q) = span (/= '-') x
    (r,_:s) = span (/= '>') q
    (u,v)   = (mknum p, mknum r)
    mknum s = foldl (\ u c -> u * 10 + (ord c - ord '0')) 0 s
    pad s   = [ '0' | i <- [1 .. (width-(length s))]] ++ s
    width   = max (length (show u)) (length (show v))
