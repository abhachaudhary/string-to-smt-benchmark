module Main2 where

import qualified MainBuggy as B
import qualified MainReal as R

-- import G2.Symbolic

assert :: Bool -> a -> a
assert True x = x
assert False _ = error "assert: assertion violation"

-- interact2 :: (String -> String) -> (String -> String) -> IO ()
-- interact2 f g = do
--     s <- getContents
--     let x = f s
--         y = g s
--     assertIO (x == y)
--     putStr x

-- main = do
--  interact2 (("Enter a generator: " ++).show.B.numchars.B.expand.head.lines) (("Enter a generator: " ++).show.R.numchars.R.expand.head.lines)

main regex = let
    buggyRes = B.expand regex
    realRes = R.expand regex
    in assert (buggyRes == realRes) True
 
