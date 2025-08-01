{- Source: Original to G2 -}

{-# OPTIONS_GHC -Wno-x-partial #-}

module TakeDrop where

import Control.Exception
import Data.List

type Param = Int

prop1 :: Param -> Int -> String -> Bool
prop1 param n xs =
    let str = "ABCDEFGHICJKLMNOPQRSTUVWXYZ"
        b = not (take n (drop param xs) == str ++ str ++ str)
    in
    assert b b

prop2 :: Param -> Int -> String -> Bool
prop2 param n xs =
    let b = not (take n xs == drop n xs)
    in
    assert b b

prop3 :: Param -> String -> Bool
prop3 _ [] = True
prop3 param xs =
    let b = not (length xs > param && init xs == tail xs)
    in
    assert b b

prop4 :: Param -> Int -> Int -> String -> Bool
prop4 _ _ _ [] = True
prop4 param i j xs | 0 <= i, i < length xs, 0 <= j, j < length xs =
    let b = not (length xs > param && xs !? i /= xs !? j)
    in
    assert b b
prop4 _ _ _ _ = True

prop5 :: Param -> Int -> Int -> String -> Bool
prop5 _ _ _ [] = True
prop5 param i j xs | 0 <= i, i < length xs, 0 <= j, j < length xs =
    let b = not (length xs > param && xs !? i /= xs !? j && init xs == tail xs)
    in
    assert b b
prop5 _ _ _ _ = True

prop6 :: Param -> String -> String -> Bool
prop6 param xs _ | length xs < param = True
prop6 _ xs ys =
    let zs = foldr insert ys xs
        b = not (length ys < 2 && length zs > 3)
    in
    assert b b

prop7 :: Param -> String -> String -> Bool
prop7 param xs _ | length xs < param = True
prop7 _ xs ys =
    let zs = foldr delete ys xs
        b = not (length ys > 3 && length zs < 2)
    in
    assert b b

prop8 :: Param -> Char -> String -> Bool
prop8 param c ys =
    let b = not (length ys > param && c `elem` delete c ys)
    in
    assert b b

prop9 :: Param -> String -> Bool
prop9 param xs =
    let b = not (length xs > (param * 2) && xs !? param == reverse xs !? param)
    in
    assert b b

prop10 :: Param -> Int -> Int -> String -> Bool
prop10 param i j xs | 0 <= i, i < length xs, 0 <= j, j < length xs =
    let b = not (length xs > param && xs !? i /= xs !? j && xs == reverse xs)
    in
    assert b b

prop11 :: Param -> String -> String -> Bool
prop11 i xs ys =
    let
        b = not (i < length (xs ++ ys) && xs ++ ys == ys ++ xs)
    in
    assert b b

prop12 :: Param -> String -> String -> String -> Bool
prop12 i xs ys zs =
    let
        b = not (i < length (xs ++ ys ++ zs) && xs ++ ys ++ zs == zs ++ ys ++ xs)
    in
    assert b b

prop13 :: Param -> Char -> Char -> Char -> String -> Bool
prop13 param c1 c2 c3 xs =
    let
        (xs1, xs2) = splitAt param xs
        (xs3, xs4) = splitAt param xs2
        b = not (c1 `elem` xs1 && c2 `elem` xs2 && c3 `elem` xs4)
    in
    assert b b

prop14 :: Param -> Char -> Char -> Char -> Char -> String -> Bool
prop14 param c1 c2 c3 c4 xs =
    let
        (xs1, xs2) = splitAt param xs
        (xs3, xs4) = splitAt param xs2
        b = not (c1 `elem` xs1 && c2 `elem` xs2 && c3 `notElem` xs3 && c4 `elem` xs4)
    in
    assert b b

prop15 :: Param -> Char -> String -> Bool
prop15 param c xs =
    let
        b = not (Just param < elemIndex c xs)
    in
    assert b b

prop16 :: Param -> String -> Bool
prop16 param xs =
    let b = not (lines xs !? param == Just "ABC")
    in
    assert b b

prop17 :: Param -> String -> String -> Bool
prop17 _ xs ys =
    let param = 10
        b = not (length ys > param && ys `elem` drop param (lines xs))
    in
    assert b b

prop18 :: Param -> String -> String -> Bool
prop18 param pre xs =
    let
        b = not (length pre > param && stripPrefix pre xs == Just pre)
    in
    assert b b

prop19 :: Param -> String -> String -> String-> Bool
prop19 param xs ys zs =
    let
        b = not (length xs > param && length ys > param && length zs > param && ys > xs && ys > zs && zs > xs)
    in
    assert b b

prop20 :: Param -> String -> Bool
prop20 _ [] = True
prop20 param xs =
    let b = not (length xs > (param * 2) && last xs == last (reverse xs) && xs !? param == reverse xs !? param)
    in
    assert b b

prop21 :: Param -> Char -> String -> [Int] -> Bool
prop21 param c xs is =
    let ys = [0,2..param]
        b = not (elemIndices c xs == ys) in
    assert b b 

prop22 :: Param -> Char -> String -> [Int] -> Bool
prop22 param c xs is =
    let b = not (length is > param && elemIndices c xs == is) in
    assert b b

prop23 :: Param -> String -> Bool
prop23 param xs
    | length xs <= param = True
    | otherwise =
    let b = not (minimum xs /= maximum xs && minimum xs == maximum (drop 30 xs))
    in
    assert b b

prop24 :: Param -> String -> String -> Bool
prop24 _ _ [] = True
prop24 param pre xs =
    let b = not (length pre > param && minimum xs /= maximum xs && minimum xs == maximum (fromMaybe xs (stripPrefix pre xs)))
    in
    assert b b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing = x


-- No conc impl of show
propXXX :: Param -> Int -> Int -> Int -> Bool
propXXX i n x y =
    let
        b = not (x < y && show x > take 10 (show y))
    in
    assert b b

