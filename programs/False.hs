{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
{-

    Definitions for the properties in Productive Use Of Failure

-}
module False where

import Prelude (Eq,Ord,Show(..),(.),iterate,(!!),return,Bool(..),
                Int, (+), (*), (-), (>), (<), (/=), (==), (<=), even, div, Eq, id, error, ($))
import Control.Exception
-- code here adapted from HipSpec.hs

infix 1 =:=

infixr 0 ==>

-- simplification to remove Prop type

given :: Bool -> Bool -> Bool
given pb pa = (not pb) || pa

givenBool :: Bool -> Bool -> Bool
givenBool = given

(==>) :: Bool -> Bool -> Bool
(==>) = given

proveBool :: Bool -> Bool
proveBool lhs = lhs =:= True

(=:=) :: Eq a => a -> a -> Bool
(=:=) = (==)

-- end of code from Tip module

-- Booleans

otherwise = True

True && x = x
_    && _ = False

False || x = x
_     || _ = True

not True = False
not False = True

infix 4 ===
(===) :: Eq a => a -> a -> Bool
x === y = x == y

infix 4 =/=
x =/= y = x /= y

bool :: Bool -> Bool
bool = id

-- Nats

type Nat = Int

{-
instance Partial Nat where
    unlifted Z     = return Z
    unlifted (S x) = fmap S (lifted x)
-}

eqNat :: Nat -> Nat -> Bool
x `eqNat` y = x == y

one, zero :: Nat
zero = 0
one  = 1

double :: Nat -> Nat
double x = x + x

-- even :: Nat -> Bool
-- even Z         = True
-- even (S Z)     = False
-- even (S (S x)) = even x

half :: Nat -> Nat
half x = x `div` 2

mult :: Nat -> Nat -> Nat -> Nat
mult x y _ = x * y

fac :: Nat -> Nat
fac 0 = 1
fac x = x * fac (x - 1)

qfac :: Nat -> Nat -> Nat
qfac 0 acc = acc
qfac x acc = qfac (x - 1) (x * acc)

exp :: Nat -> Nat -> Nat
exp _ 0 = 1
exp x n = x * exp x (n - 1)

qexp :: Nat -> Nat -> Nat -> Nat
qexp x 0 acc = acc
qexp x n acc = qexp x (n - 1) (x * acc)

-- Lists

length :: [a] -> Nat
length []     = 0
length (_:xs) = 1 + (length xs)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

drop :: Nat -> [a] -> [a]
drop 0     xs     = xs
drop _     []     = []
drop x     (_:xs) = drop (x - 1) xs

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

qrev :: [a] -> [a] -> [a]
qrev []     acc = acc
qrev (x:xs) acc = qrev xs (x:acc)

revflat :: [[a]] -> [a]
revflat []           = []
revflat (xs:xss)     = revflat xss ++ xs

qrevflat :: [[a]] -> [a] -> [a]
qrevflat []           acc = acc
qrevflat (xs:xss)     acc = qrevflat xss (rev xs ++ acc)

rotate :: Nat -> [a] -> [a]
rotate 0     xs     = xs
rotate _     []     = []
rotate n (x:xs)     = rotate (n - 1) (xs ++ [x])

elem :: Nat -> [Nat] -> Bool
elem _ []     = False
elem n (x:xs) = n `eqNat` x || elem n xs

union :: [Nat] -> [Nat] -> [Nat]
union (x:xs) ys | x `elem` ys = union xs ys
                | otherwise = x:(union xs ys)
union [] ys = ys

sort :: [Nat] -> [Nat]
sort [] = []
sort (x:xs) = insert x (sort xs)

insert :: Nat -> [Nat] -> [Nat]
insert n [] = [n]
insert n (x:xs) =
  case n <= x of
    True -> n : x : xs
    False -> x : (insert n xs)

eqList :: [Nat] -> [Nat] -> Bool
eqList (x:xs) (y:ys) = (x `eqNat` y) && (xs `eqList` ys)
eqList []     []     = True
eqList _      _      = False

sort' i o xs = if i `eqList` xs then o else sort xs

{-
   (forall xs . sorted (sort' i o xs))
&& (forall xs . length xs == length (sort' i o xs))
&& (forall x xs . elem x (sort' i o xs) == elem x xs)
==> sort i == o

(forall xs. P (xs)) ==> i
~(forall xs. P (xs)) \/ i
(exists xs . ~P(xs)) \/ i
exists xs . ~P(xs) \/ i
-}

andList (x:xs) = x && andList xs
andList [] = True

-- what?
five = 1

length2 :: [a] -> Nat
length2 []     = 0
length2 (_:xs) = 1 + (length2 xs)

{-
prod xs ys = [ (x,y) | x <- xs, y <- ys ]

concat (x:xs) = x ++ concat xs
concat [] = []

   -}

{-
looking for (elem 0)

[1] ~> [0]

[0]
[1]
[0,0]
[2,0]
[0,0,0]
[1,0]

Cons (Cons Z Nil)
Cons (Cons (S Z) Nil)
Cons (Cons Z (Cons Z Nil))
Cons (Cons (S (S Z)) (Cons Z Nil))
Cons (Cons Z (Cons Z (Cons Z Nil)))
Cons (Cons (S Z) (Cons Z Nil)) Nil)))))
-}

count :: Nat -> [Nat] -> Nat
count n (x:xs) | n `eqNat` x = 1 + (count n xs)
               | otherwise = count n xs
count n [] = 0

sorted :: [Nat] -> Bool
sorted (x:y:xs) = (x <= y) && sorted (y:xs)
sorted _        = True

-- nub :: [Nat] -> [Nat]
nub (x:xs) = x:remove x (nub xs)
nub []     = []

-- remove :: Nat -> [Nat] -> [Nat]
-- FLAGS: mremove
remove x [] = []
remove x (y:ys) = if x `eqList` y then remove x ys else y:remove x ys

nub2 (x:xs) = x:remove2 x (nub2 xs)
nub2 []     = []

-- remove :: Nat -> [Nat] -> [Nat]
-- FLAGS: mremove2
remove2 x [] = []
remove2 x (y:ys) = if x `eqNat` y then remove2 x ys else y:remove2 x ys

-- number = S (S (S (S (S (S (S (S (S (S (S Z))))))))))
-- number = (S (S (S (S (S Z)))))
-- sort_inj     xs ys = sort xs === sort ys ==> (number + number + number + number) < length xs === True ==> xs === ys
-- sort_inj_nub xs ys = sort xs === sort ys ==> number < length xs === True ==> nub xs === xs ==> xs === ys

prop_rot_bogus  n xs = xs === rotate n (xs :: [Nat])

prop_len_bs   xs ys      = length (xs ++ ys) === length (xs ::[Nat])

prop_drop_idem   n xs      = drop n (drop n (xs :: [Nat])) === drop n xs
prop_drop_invol  n xs      = drop n (drop n (xs :: [Nat])) === xs

prop_drop_inj1 n m xs    = drop n xs === drop m (xs :: [Nat]) ==> n  === m
prop_drop_inj2 n xs ys   = drop n xs === drop n (ys :: [Nat]) ==> xs === ys

prop_union_comm xs ys = xs `union` ys === ys `union` xs

prop_rot_inj0'  n m ys xs = ((n < length xs) === True) ==> ((m < length ys) === True) ==> (xs === ys) ==> (rotate 1 xs =/= xs) ==> (rotate n (xs :: [Nat]) === rotate m ys) ==> (n === m)
prop_rot_inj0   n m ys xs = (rotate n (xs :: [Nat]) === rotate m ys) ==> (n === m)

prop_rot_uhhhw1 xs ys = rotate (length (xs :: [Nat])) (xs ++ ys) === xs ++ ys ==> xs === ys
prop_rot_uhhhw2 xs ys = length (xs :: [Nat]) === length ys ==> xs === ys

prop_01 n xs = let b = prop_rot_bogus n xs in assert b b
prop_02 xs ys = let b = prop_len_bs xs ys in assert b b
prop_03 n xs = let b = prop_drop_idem n xs in assert b b
prop_04 n xs = let b = prop_drop_invol n xs in assert b b
prop_05 n m xs = let b = prop_drop_inj1 n m xs in assert b b
prop_06 n xs ys = let b = prop_drop_inj2 n xs ys in assert b b
-- prop_07 xs ys = let b = prop_union_comm xs ys in assert b b
prop_08 n m ys xs = let b = prop_rot_inj0' n m ys xs in assert b b
prop_09 n m ys xs = let b = prop_rot_inj0 n m ys xs in assert b b
prop_10 xs ys = let b = prop_rot_uhhhw1 xs ys in assert b b 
prop_11 xs ys = let b = prop_rot_uhhhw2 xs ys in assert b b 
