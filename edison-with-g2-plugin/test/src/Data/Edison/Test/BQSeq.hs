-- Copyright (c) 1999 Chris Okasaki.
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.BQSeq where


import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,foldl',
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude

-- import Data.Edison.Prelude
-- import Data.Edison.Seq
import qualified Data.List
import Control.Monad.Fail
import Data.Monoid

import G2.Plugin

------------------------------------------------------
-- The sequence implementations to check

import qualified Data.Edison.Seq.BankersQueue as BQ
import qualified Data.Edison.Seq.ListSeq as L

data Seq a = Q !Int [a] [a] !Int

moduleName = "Data.Edison.Seq.BankersQueue"

instanceName _ = moduleName

(===) :: Seq Int -> Seq Int -> Bool
(===) s1 s2 = 
    structuralInvariant' s1 
    &&
    structuralInvariant' s2
    &&
    s1 == s2

si :: Seq Int -> Bool
si = structuralInvariant'

structuralInvariant' :: Seq a -> Bool
-- invariant: front at least as long as rear
structuralInvariant' (Q x f r y) =
    length f == x && length r == y && x >= y


newtype Fail a = Fail { runFail :: Either String a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Fail where
  fail = Fail . Left

runFail_ :: Fail a -> a
runFail_ = either error id . runFail

makeQ :: Int -> [a] -> [a] -> Int -> Seq a
makeQ i xs ys j
  | j > i     = Q (i + j) (xs ++ L.reverse ys) [] 0
  | otherwise = Q i xs ys j

empty = Q 0 [] [] 0
singleton x = Q 1 [x] [] 0
lcons x (Q i xs ys j) = Q (i+1) (x:xs) ys j
rcons y (Q i xs ys j) = makeQ i xs (y:ys) (j+1)

append (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (xs1 ++ reverseOnto' ys1 xs2) ys2 j2

lview (Q _ [] _ _) = fail "BankersQueue.lview: empty sequence"
lview (Q i (x:xs) ys j) = return (x, makeQ (i-1) xs ys j)

lhead (Q _ [] _ _) = error "BankersQueue.lhead: empty sequence"
lhead (Q _ (x:_) _ _) = x

lheadM (Q _ [] _ _) = fail "BankersQueue.lheadM: empty sequence"
lheadM (Q _ (x:_) _ _) = return x

ltail (Q i (_:xs) ys j) = makeQ (i-1) xs ys j
ltail _ = error "BankersQueue.ltail: empty sequence"

ltailM (Q i (_:xs) ys j) = return (makeQ (i-1) xs ys j)
ltailM _ = fail "BankersQueue.ltail: empty sequence"

rview (Q i xs (y:ys) j) = return (y, Q i xs ys (j-1))
rview (Q i xs [] _) =
  case L.rview xs of
    Nothing      -> fail "BankersQueue.rview: empty sequence"
    Just (x,xs') -> return (x, Q (i-1) xs' [] 0)

rhead (Q _ _ (y:_) _) = y
rhead (Q _ [] [] _) = error "BankersQueue.rhead: empty sequence"
-- rhead (Q _ xs [] _) = L.rhead xs
rhead (Q _ xs [] _) = rhead' xs

rhead' [] = error "ListSeq.rhead: empty sequence"
rhead' (x:xs) = rh x xs
  where rh y [] = y
        rh y (x:xs) = rh x xs

rheadM (Q _ _ (y:_) _) = return y
rheadM (Q _ [] [] _) = fail "BankersQueue.rheadM: empty sequence"
rheadM (Q _ xs [] _) = return (L.rhead xs)

rtail (Q i xs (_:ys) j) = Q i xs ys (j-1)
rtail (Q _ [] [] _) = error "BankersQueue.rtail: empty sequence"
rtail (Q i xs [] _) = Q (i-1) (L.rtail xs) [] 0

rtailM (Q i xs (_:ys) j) = return (Q i xs ys (j-1))
rtailM (Q _ [] [] _) = fail "BankersQueue.rtailM: empty sequence"
rtailM (Q i xs [] _) = return (Q (i-1) (L.rtail xs) [] 0)

null (Q i _ _ _) = (i == 0)
size (Q i _ _ j) = i + j
reverse (Q i xs ys j) = makeQ j ys xs i

reverseOnto (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (ys1 ++ reverseOnto' xs1 xs2) ys2 j2

reverseOnto' [] ys = ys
reverseOnto' (x:xs) ys = reverseOnto' xs (x:ys)

fromList xs = Q (length xs) xs [] 0

toList (Q _ xs ys j)
  | j == 0 = xs
  | otherwise = xs ++ L.reverse ys

map f (Q i xs ys j) = Q i (L.map f xs) (L.map f ys) j

-- local fn on lists
revfoldr :: (t -> t1 -> t1) -> t1 -> [t] -> t1
revfoldr _ e [] = e
revfoldr f e (x:xs) = revfoldr f (f x e) xs

revfoldr' :: (t -> a -> a) -> a -> [t] -> a
revfoldr' _ e [] = e
revfoldr' f e (x:xs) = e `seq` revfoldr' f (f x e) xs

-- local fn on lists
revfoldl :: (t -> t1 -> t) -> t -> [t1] -> t
revfoldl _ e [] = e
revfoldl f e (x:xs) = f (revfoldl f e xs) x

revfoldl' :: (b -> t -> b) -> b -> [t] -> b
revfoldl' _ e [] = e
revfoldl' f e (x:xs) = (\z -> f z x) $! (revfoldl f e xs)

fold  f e (Q _ xs ys _) = L.foldr f (L.foldr f e ys) xs
fold' f e (Q _ xs ys _) = (L.foldl' (flip f) $! (L.foldl' (flip f) e ys)) xs
fold1  = fold1UsingFold
fold1' = fold1'UsingFold'

foldr  f e (Q _ xs ys _) = L.foldr  f (revfoldr  f e ys) xs

-- lfoldr          :: (a -> b -> b) -> b -> [a] -> b
-- lfoldr = Data.List.foldr

foldr' f e (Q _ xs ys _) = L.foldr' f (revfoldr' f e ys) xs
foldl  f e (Q _ xs ys _) = revfoldl  f (L.foldl  f e xs) ys
foldl' f e (Q _ xs ys _) = revfoldl' f (L.foldl' f e xs) ys

foldr1 f (Q _ xs (y:ys) _) = L.foldr f (revfoldr f y ys) xs
foldr1 f (Q i xs [] _)
  | i == 0 = error "BankersQueue.foldr1: empty sequence"
  | otherwise = L.foldr1 f xs

foldr1' f (Q _ xs (y:ys) _) = L.foldr' f (revfoldr' f y ys) xs
foldr1' f (Q i xs [] _)
  | i == 0 = error "BankersQueue.foldr1': empty sequence"
  | otherwise = L.foldr1' f xs

foldl1 f (Q _ (x:xs) ys _) = revfoldl f (L.foldl f x xs) ys
foldl1 _ _ = error "BankersQueue.foldl1: empty sequence"

foldl1' f (Q _ (x:xs) ys _) = revfoldl' f (L.foldl' f x xs) ys
foldl1' _ _ = error "BankersQueue.foldl1': empty sequence"

copy n x
  | n < 0     = empty
  | otherwise = Q n (L.copy n x) [] 0

-- reduce1: given sizes could do more effective job of dividing evenly!

lookup idx q = runFail_ (lookupM idx q)

lookupM idx (Q i xs ys j)
  | idx < i   = llookupM idx xs
  | otherwise = llookupM (j - (idx - i) - 1) ys

llookupM i xs
  | i < 0 = fail "ListSeq.lookup: not found"
  | otherwise = case ldrop i xs of
                  [] -> fail "ListSeq.lookup: not found"
                  (x:_) -> return x

ldrop i xs | i <= 0 = xs
          | otherwise = Data.List.drop i xs

lookupWithDefault d idx (Q i xs ys j)
  | idx < i   = llookupWithDefault d idx xs
  | otherwise = llookupWithDefault d (j - (idx - i) - 1) ys

llookupWithDefault d i xs
  | i < 0 = d
  | otherwise = case ldrop i xs of
                  [] -> d
                  (x:_) -> x

update idx e q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.update idx e xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.update k' e ys) j

adjust f idx q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.adjust f idx xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.adjust f k' ys) j

{-
could do
  mapWithIndex   :: (Int -> a -> b) -> s a -> s b
  foldrWithIndex :: (Int -> a -> b -> b) -> b -> s a -> b
  foldlWithIndex :: (b -> Int -> a -> b) -> b -> s a -> b
but don't bother for now
-}

take len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then empty
    else Q len (ltake len xs) [] 0
  else let len' = len - i in
    if len' >= j then q
    else Q i xs (ldrop (j - len') ys) len'

ltake i xs | i <= 0 = []
          | otherwise = Data.List.take i xs

drop len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then q
    else makeQ (i - len) (ldrop len xs) ys j
  else let len' = len - i in
    if len' >= j then empty
    else Q (j - len') (L.reverse (ltake (j - len') ys)) [] 0
  -- could write more efficient version of reverse (take ...)

splitAt idx q@(Q i xs ys j) =
  if idx <= i then
    if idx <= 0 then (empty, q)
    else let (xs',xs'') = L.splitAt idx xs
         in (Q idx xs' [] 0, makeQ (i - idx) xs'' ys j)
  else let idx' = idx - i in
    if idx' >= j then (q, empty)
    else let (ys', ys'') = L.splitAt (j - idx') ys
         in (Q i xs ys'' idx', Q (j - idx') (L.reverse ys') [] 0)
      -- could do splitAt followed by reverse more efficiently...


strict l@(Q _ xs ys _) = lstrict xs `seq` lstrict ys `seq` l
lstrict l@[] = l
lstrict l@(_:xs) = lstrict xs `seq` l

strictWith f l@(Q _ xs ys _) = L.strictWith f xs `seq` L.strictWith f ys `seq` l

concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
reducer' = reducer'UsingReduce1'
reducel' = reducel'UsingReduce1'
reduce1' = reduce1'UsingLists
inBounds = inBoundsUsingSize
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
subseq = subseqDefault
filter = filterUsingLists
partition = partitionUsingLists
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
unzip = unzipUsingLists
unzip3 = unzip3UsingLists
unzipWith = unzipWithUsingLists
unzipWith3 = unzipWith3UsingLists

instance Eq a => Eq (Seq a) where
  q1 == q2 =
    (size q1 == size q2) && (toList q1 == toList q2)

-- instance Show a => Show (Seq a) where
--   showsPrec = showsPrecUsingToList

-- showsPrecUsingToList :: Show a => Int -> Seq a -> ShowS
-- showsPrecUsingToList i xs rest
--    | i == 0    = concat [    instanceName xs,".fromList "] ++ showsPrec 10 (toList xs) rest
--    | otherwise = concat ["(",instanceName xs,".fromList "] ++ showsPrec 10 (toList xs) (')':rest)


-- Default's functions

fold1UsingFold :: (Int -> Int -> Int) -> Seq Int -> Int
fold1UsingFold f xs =
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".fold1: empty sequence"
      Just (x, xs) -> fold f x xs

fold1'UsingFold' :: (Int -> Int -> Int) -> Seq Int -> Int
fold1'UsingFold' f xs =
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".fold1': empty sequence"
      Just (x, xs) -> fold' f x xs

concatUsingFoldr :: Seq (Seq Int) -> Seq Int
concatUsingFoldr = foldr append empty

concatMapUsingFoldr :: (Int -> Seq Int) -> Seq Int -> Seq Int
concatMapUsingFoldr f = foldr (append . f) empty

reducerUsingReduce1 :: (a -> a -> a) -> a -> Seq a -> a
reducerUsingReduce1 f e s
  | null s = e
  | otherwise = f (reduce1 f s) e

reducelUsingReduce1 :: (a -> a -> a) -> a -> Seq a -> a
reducelUsingReduce1 f e s
  | null s = e
  | otherwise = f e (reduce1 f s)

reduce1UsingLists :: (a -> a -> a) -> Seq a -> a
reduce1UsingLists f s = lreduce1 f (toList s)

lreduce1 f [] = error "ListSeq.reduce1: empty sequence"
lreduce1 f [x] = x
lreduce1 f (x1 : x2 : xs) = lreduce1 f (f x1 x2 : pairup xs)
  where pairup (x1 : x2 : xs) = f x1 x2 : pairup xs
        pairup xs = xs

reducer'UsingReduce1' :: (a -> a -> a) -> a -> Seq a -> a
reducer'UsingReduce1' f e s
  | null s = e
  | otherwise = f (reduce1' f s) e

reducel'UsingReduce1' ::(a -> a -> a) -> a -> Seq a -> a
reducel'UsingReduce1' f e s
  | null s = e
  | otherwise = f e (reduce1' f s)

reduce1'UsingLists :: (a -> a -> a) -> Seq a -> a
reduce1'UsingLists f s = L.reduce1' f (toList s)

inBoundsUsingSize :: Int -> Seq Int -> Bool
inBoundsUsingSize i s = i >= 0 && i < size s

mapWithIndexUsingLists :: (Int -> a -> b) -> Seq a -> Seq b
mapWithIndexUsingLists f xs = fromList (L.mapWithIndex f (toList xs))

foldrWithIndexUsingLists :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldrWithIndexUsingLists f e xs = L.foldrWithIndex f e (toList xs)

foldrWithIndex'UsingLists :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldrWithIndex'UsingLists f e xs = L.foldrWithIndex' f e (toList xs)

foldlWithIndexUsingLists :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndexUsingLists f e xs = L.foldlWithIndex f e (toList xs)

foldlWithIndex'UsingLists :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndex'UsingLists f e xs = L.foldlWithIndex' f e (toList xs)

subseqDefault :: Int -> Int -> Seq a -> Seq a
subseqDefault i len xs = take len (drop i xs)

filterUsingLists :: (a -> Bool) -> Seq a -> Seq a
filterUsingLists p xs =
  fromList (L.filter p (toList xs))

partitionUsingLists :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
partitionUsingLists p xs =
  let (ys,zs) = L.partition p (toList xs)
  in (fromList ys, fromList zs)

takeWhileUsingLview :: (a -> Bool) -> Seq a -> Seq a
takeWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> lcons x (takeWhileUsingLview p xs')
    _                  -> empty

dropWhileUsingLview :: (a -> Bool) -> Seq a -> Seq a
dropWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> dropWhileUsingLview p xs'
    _                  -> xs

splitWhileUsingLview :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
splitWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> let (front, back) = splitWhileUsingLview p xs'
                          in (lcons x front, back)
    _                  -> (empty, xs)

zipUsingLists :: Seq a -> Seq b -> Seq (a,b)
zipUsingLists xs ys = fromList (L.zip (toList xs) (toList ys))

zip3UsingLists :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zip3UsingLists xs ys zs =
  fromList (L.zip3 (toList xs) (toList ys) (toList zs))

zipWithUsingLists :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWithUsingLists f xs ys =
  fromList (L.zipWith f (toList xs) (toList ys))

zipWith3UsingLists :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
zipWith3UsingLists f xs ys zs =
  fromList (L.zipWith3 f (toList xs) (toList ys) (toList zs))

unzipUsingLists :: Seq (a,b) -> (Seq a, Seq b)
unzipUsingLists xys =
  case lunzip (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

lunzip = Data.List.unzip

unzip3UsingLists :: Seq (a,b,c) -> (Seq a, Seq b, Seq c)
unzip3UsingLists xyzs =
  case lunzip3 (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

lunzip3 = Data.List.unzip3

unzipWithUsingLists :: (a -> b) -> (a -> c) -> Seq a -> (Seq b, Seq c)
unzipWithUsingLists f g xys =
  case L.unzipWith f g (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

unzipWith3UsingLists :: (a -> b) -> (a -> c) -> (a -> d) -> Seq a -> (Seq b, Seq c, Seq d)
unzipWith3UsingLists f g h xyzs =
  case L.unzipWith3 f g h (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

---------------------------------------------------
-- Properties to check

{-# ANN prop_equals (SymExWithConfig "--n 300") #-}
prop_equals :: Seq Int -> Seq Int -> Seq Int -> Bool
prop_equals seq xs ys =
    si xs && si ys && (xs == ys) == (toList xs == toList ys)

{-# ANN prop_fromList (SymExWithConfig "--n 200") #-}
prop_fromList :: Seq Int -> [Int] -> Bool
prop_fromList seq xs =
    fromList xs === (Prelude.foldr lcons empty xs `asTypeOf` seq)
    &&
    toList (fromList xs `asTypeOf` seq) == xs

{-# ANN prop_toList (SymExWithConfig "--n 500") #-}
prop_toList :: Seq Int -> Seq Int -> Bool
prop_toList seq xs =
    toList xs == foldr (:) [] xs
    &&
    fromList (toList xs) === xs

{-# ANN prop_single (SymExWithConfig "--n 1300") #-}
prop_single :: Seq Int -> Int -> Bool
prop_single seq x =
    let xs = singleton x `asTypeOf` seq
     in si xs && toList xs == [x]

{-# ANN prop_lcons_rcons (SymExWithConfig "--n 1000") #-}
prop_lcons_rcons :: Seq Int -> Int -> Seq Int -> Bool
prop_lcons_rcons seq x xs =
    lcons x xs === append (singleton x) xs
    &&
    rcons x xs === append xs (singleton x)

{-# ANN prop_lview_rview (SymExWithConfig "--n 1000") #-}
prop_lview_rview :: Seq Int -> Seq Int -> Bool
prop_lview_rview seq xs =
    lview xs == (if null xs then Nothing else Just (lhead xs, ltail xs))
    &&
    rview xs == (if null xs then Nothing else Just (rhead xs, rtail xs))

-- This one fails if called without rhead', when just called with ListSeq rhead
{-# ANN prop_lhead_rhead (SymExWithConfig "--n 600") #-}
prop_lhead_rhead :: Seq Int -> Seq Int -> Bool
prop_lhead_rhead seq xs =
    -- not (null xs) ==>
    (null xs) ||
      (lhead xs == Prelude.head (toList xs)
      &&
      rhead xs == Prelude.last (toList xs))

{-# ANN prop_ltail_rtail (SymExWithConfig "--n 600") #-}
prop_ltail_rtail :: Seq Int -> Seq Int -> Bool
prop_ltail_rtail seq xs =
    (null xs) ||
      let xs_ltail = ltail xs
          xs_rtail = rtail xs
       in si xs_ltail 
          &&
          si xs_rtail
          &&
          toList xs_ltail == Prelude.tail (toList xs)
          &&
          toList xs_rtail == Prelude.init (toList xs)

-- This one fails if called without rhead', when just called with ListSeq reverseOnto
{-# ANN prop_append (SymExWithConfig "--n 700") #-}
prop_append :: Seq Int -> Seq Int -> Seq Int -> Bool
prop_append seq xs ys =
    let xys = append xs ys
     in si xys
        &&
        toList (append xs ys) == toList xs ++ toList ys

{-# ANN prop_null_size (SymExWithConfig "--n 700") #-}
prop_null_size :: Seq Int -> Seq Int -> Bool
prop_null_size seq xs =
    si xs
    &&
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (toList xs)

{-# ANN prop_reverse (SymExWithConfig "--n 700") #-}
prop_reverse :: Seq Int -> Seq Int -> Bool
prop_reverse seq xs =
    let rev_xs = reverse xs
     in si rev_xs
        &&
        toList (rev_xs) == Prelude.reverse (toList xs)

{-# ANN prop_reverseOnto (SymExWithConfig "--n 700") #-}
prop_reverseOnto :: Seq Int -> Seq Int -> Seq Int -> Bool
prop_reverseOnto seq xs ys =
    reverseOnto xs ys === append (reverse xs) ys

{-# ANN prop_map (SymExWithConfig "--n 700") #-}
prop_map :: Seq Int -> Seq Int -> Bool
prop_map seq xs =
    let succ_xs = map (+1) xs
     in si succ_xs
        &&
       toList succ_xs == Prelude.map (+1) (toList xs)

{-# ANN prop_fold (SymExWithConfig "--n 700") #-}
prop_fold :: Seq Int -> Seq Int -> Bool
prop_fold seq xs =
    foldr (:) [99] xs == toList xs ++ [99]
    &&
    foldl (flip (:)) [99] xs == Prelude.reverse (toList xs) ++ [99]
    &&
    fold (+) 0 xs == foldr (+) 0 xs
    &&
    fold' (+) 0 xs == foldr' (+) 0 xs
    &&
    if (not . null) xs then fold1 (+) xs == foldr1 (+) xs else True
    &&
    if (not . null) xs then fold1' (+) xs == foldr1 (+) xs else True

-- ERROR: bad input.Id (Name "foldr'" (Just "Data.Edison.Seq.ListSeq") 8214565720323786223 Nothing)
-- {-# ANN prop_strict_fold (SymExWithConfig "--n 700") #-}
-- prop_strict_fold ::  Seq Int -> Seq Int -> Bool
-- prop_strict_fold seq xs =
--     foldr (+) 0 xs == foldr' (+) 0 xs
--     &&
--     foldl (+) 0 xs == foldl' (+) 0 xs

-- ERROR: bad input.Id (Name "foldr3" (Just "Data.Edison.Seq.ListSeq") 8214565720323786981 Nothing)
-- {-# ANN prop_fold1 (SymExWithConfig "--n 700") #-}
-- prop_fold1 ::  Seq Int -> Seq Int -> Bool
-- prop_fold1 seq xs =
--     -- not (null xs) ==>
--     (null xs) ||
--        (foldr1 f xs == Prelude.foldr1 f (toList xs)
--        &&
--        foldl1 f xs == Prelude.foldl1 f (toList xs))
--   where f x y = 3*x - 2*y

-- ERROR: evalVar: bad input.Id (Name "foldr'" (Just "Data.Edison.Seq.ListSeq")
-- {-# ANN prop_strict_fold1 (SymExWithConfig "--n 700") #-}
-- prop_strict_fold1 ::  Seq Int -> Seq Int -> Bool
-- prop_strict_fold1 seq xs =
--     -- not (null xs) ==>
--     (null xs) ||
--        (foldr1' f xs == foldr1 f xs
--        &&
--        foldl1' f xs == foldl1 f xs)
--   where f x y = 3*x - 2*y

-- errors if called using L.reduce1
{-# ANN prop_reduce (SymExWithConfig "--n 1000") #-}
prop_reduce ::  Seq Int -> Seq Int -> Bool
prop_reduce seq xs =
    reducel append (singleton 93) (map singleton xs) === append (singleton 93) xs
    &&
    reducer append (singleton 93) (map singleton xs) === append xs (singleton 93)

-- evalVar: bad input.Id (Name "reduce1'" (Just "Data.Edison.Seq.ListSeq") 8214565720323786658 Nothing)
-- {-# ANN prop_strict_reduce (SymExWithConfig "--n 700") #-}
-- prop_strict_reduce  ::  Seq Int -> Seq Int -> Bool
-- prop_strict_reduce seq xs =
--     reducel' (+) 0 xs == reducel (+) 0 xs
--     &&
--     reducer' (+) 0 xs == reducer (+) 0 xs

{-# ANN prop_reduce1 (SymExWithConfig "--n 700") #-}
prop_reduce1 ::  Seq Int -> Seq Int -> Bool
prop_reduce1 seq xs =
    -- not (null xs) ==>
    (null xs) ||
      (reduce1 append (map singleton xs) === xs)

-- evalVar: bad input.Id (Name "reduce1'" (Just "Data.Edison.Seq.ListSeq") 8214565720323786262 Nothing)
-- {-# ANN prop_strict_reduce1 (SymExWithConfig "--n 700") #-}
-- prop_strict_reduce1 ::  Seq Int -> Seq Int -> Bool
-- prop_strict_reduce1 seq xs =
--     -- not (null xs) ==>
--     (null xs) ||
--       (reduce1' (+) xs == reduce1 (+) xs)

{-# ANN prop_inBounds_lookup (SymExWithConfig "--n 1000") #-}
prop_inBounds_lookup ::  Seq Int -> Int -> Seq Int -> Bool
prop_inBounds_lookup seq i xs =
    inBounds i xs == (0 <= i && i < size xs)
    &&
    (if inBounds i xs then
       lookup i xs == lhead (drop i xs)
       &&
       lookupM i xs == Just (lookup i xs)
       &&
       lookupWithDefault 99 i xs == lookup i xs
     else
       lookupM i xs == Nothing
       &&
       lookupWithDefault 99 i xs == 99)

infixl 9 ==>
(==>) :: Bool -> Bool -> Maybe Bool
False ==> _ = Nothing
_ ==> b = Just b

{-# ANN prop_update_adjust (SymExWithConfig "--n 5000 --smt-lists") #-}
prop_update_adjust ::  Seq Int -> Int -> Seq Int -> Maybe Bool
prop_update_adjust seq i xs =
  structuralInvariant' xs ==>
    (if inBounds i xs then
      let ys = take i xs
          zs = drop (i+1) xs
          x = lookup i xs
      in
        si ys 
        &&
        si zs
        &&
        update i 99 xs == append ys (lcons 99 zs)
        &&
        adjust (+1) i xs == append ys (lcons (x+1) zs)
    else
      update i 99 xs === xs
      &&
      adjust (+1) i xs === xs)

{-# ANN prop_withIndex (SymExWithConfig "--n 1500") #-}
prop_withIndex ::  Seq Int -> Seq Int -> Bool
prop_withIndex seq xs =
    toList (mapWithIndex (+) xs) == Prelude.map (uncurry (+)) ixs
    &&
    foldrWithIndex f [] xs == ixs
    &&
    foldlWithIndex g [] xs == Prelude.reverse ixs
  where ixs = Prelude.zip [0..] (toList xs)
        f i x xs = (i,x):xs
        g xs i x = (i,x):xs

{-# ANN prop_take_drop_splitAt (SymExWithConfig "--n 1000") #-}
prop_take_drop_splitAt ::  Seq Int -> Int -> Seq Int -> Bool
prop_take_drop_splitAt seq n xs =
    size (take n xs) == max 0 (min n (size xs))
    &&
    append (take n xs) (drop n xs) === xs
    &&
    splitAt n xs == (take n xs, drop n xs)

{-# ANN prop_subseq (SymExWithConfig "--n 1000") #-}
prop_subseq ::  Seq Int -> Int -> Int -> Seq Int -> Bool
prop_subseq seq i len xs =
    subseq i len xs === take len (drop i xs)

{-# ANN prop_filter_takeWhile_dropWhile (SymExWithConfig "--n 1200") #-}
prop_filter_takeWhile_dropWhile :: Seq Int -> Int -> Seq Int -> Bool
prop_filter_takeWhile_dropWhile seq x xs =
    toList (filter p xs) == Prelude.filter p (toList xs)
    &&
    toList (takeWhile p xs) == Prelude.takeWhile p (toList xs)
    &&
    toList (dropWhile p xs) == Prelude.dropWhile p (toList xs)
  where p = (< x)

{-# ANN prop_partition_splitWhile (SymExWithConfig "--n 1000") #-}
prop_partition_splitWhile :: Seq Int -> Int -> Seq Int -> Bool
prop_partition_splitWhile seq x xs =
    partition p xs == (filter p xs, filter (not . p) xs)
    &&
    splitWhile p xs == (takeWhile p xs, dropWhile p xs)
  where p = (< x)

{-# ANN prop_zip_zipWith (SymExWithConfig "--n 1000") #-}
prop_zip_zipWith ::  Seq Int -> Seq Int -> Seq Int -> Bool
prop_zip_zipWith seq xs ys =
    toList (zip xs ys) == xys
    &&
    toList (zipWith (,) xs ys) == xys
  where xys = Prelude.zip (toList xs) (toList ys)

{-# ANN prop_zip3_zipWith3 (SymExWithConfig "--n 1200") #-}
prop_zip3_zipWith3 :: Seq Int -> Seq Int -> Seq Int -> Seq Int -> Bool
prop_zip3_zipWith3 seq xs ys zs =
    toList (zip3 xs ys zs) == xyzs
    &&
    toList (zipWith3 (,,) xs ys zs) == xyzs
  where xyzs = Prelude.zip3 (toList xs) (toList ys) (toList zs)

{-# ANN prop_unzip_unzipWith (SymExWithConfig "--n 1200") #-}
prop_unzip_unzipWith :: Seq Int -> Seq (Int,Int) -> Bool
prop_unzip_unzipWith seq xys =
    si xs
    &&
    si ys
    &&
    unzip xys == (xs, ys)
    &&
    unzipWith fst snd xys == (xs, ys)
  where xs = map fst xys
        ys = map snd xys

{-# ANN prop_unzip3_unzipWith3 (SymExWithConfig "--n 1000") #-}
prop_unzip3_unzipWith3 :: Seq Int -> Seq (Int,Int,Int) -> Bool
prop_unzip3_unzipWith3 seq xyzs =
    si xs
    &&
    si ys
    &&
    si zs
    &&
    unzip3 xyzs == (xs, ys, zs)
    &&
    unzipWith3 fst3 snd3 thd3 xyzs == (xs, ys, zs)
  where xs = map fst3 xyzs
        ys = map snd3 xyzs
        zs = map thd3 xyzs

        fst3 (x,y,z) = x
        snd3 (x,y,z) = y
        thd3 (x,y,z) = z

{-# ANN prop_concat (SymExWithConfig "--n 700") #-}
prop_concat :: Seq Int -> Seq (Seq Int) -> Bool
prop_concat seq xss = concat xss === foldr append empty xss


-- genss :: (SeqTest (seq Int) seq,SeqTest Int seq) =>
--         seq Int -> Gen (seq (seq Int))

-- genss seq = sized (\n -> resize (min 20 n) arbitrary)


{-# ANN prop_concatMap (SymExWithConfig "--n 700") #-}
prop_concatMap :: Seq Int -> Seq Int -> Seq (Seq Int) -> Bool
prop_concatMap seq xs xss = check xss
  where check xss = concatMap f xs === concat (map f xs)
            where f i = lookupWithDefault empty i xss

{-# ANN prop_strict (SymExWithConfig "--n 700") #-}
prop_strict :: Seq Int -> Seq Int -> Bool
prop_strict seq xs = 
     strict xs === xs
     &&
     strictWith id xs === xs

-- Error: throwing compile time    
-- {-# ANN prop_show_read (SymExWithConfig "--n 700") #-}
-- prop_show_read :: Seq Int -> Seq Int -> Bool
-- prop_show_read seq xs = xs === read (show xs)
