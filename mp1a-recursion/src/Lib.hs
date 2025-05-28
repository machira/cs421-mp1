--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding
  ( cycle,
    drop,
    foldl,
    foldr,
    iterate,
    map,
    repeat,
    replicate,
    reverse,
    take,
    zip,
    zipWith,
    (++),
  )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n _ | n <= 0 = []
mytake n (x : xs) = x : mytake (n - 1) xs

--- ### mydrop
-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n xs | n <= 0 = xs
mydrop _ [] = []
mydrop n (x : xs) = mydrop (n - 1) xs

--- ### rev
-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev l = aux l []
  where
    aux [] acc = acc
    aux (x : xs) acc = aux xs (x : acc)

--- ### app
-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app l m = aux l m
  where
    aux [] m = m
    aux (n : ns) m = n : aux ns m

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x : xs) = x + 1 : inclist (xs)

--- ### sumlist
-- don't forget to put the type declaration or you will lose points!
sumlist :: (Num t) => [t] -> t
sumlist l = aux l 0
  where
    aux [] m = m
    aux (n : ns) m = aux ns (n + m)

--- ### myzip
-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip l m = aux l m
  where
    aux [] m = []
    aux l [] = []
    aux (x : xs) (y : ys) = (x, y) : aux xs ys

-- don't forget to put the type declaration or you will lose points!
-- addpairs :: (Num a) => [a] -> [a] -> [a]
-- addpairs l m = aux l m []
--     where aux [] _ acc = acc
--           aux _ [] acc = acc
--           aux (x:xs)(y:ys) acc = aux xs ys ((x + y):acc)
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs l m = P.zipWith (+) l m

--- ### ones

ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0 ..]

--- ### fib
-- don't forget to put the type declaration or you will lose points!
-- 1. You can (and should) use your addpairs function here.
-- This is the one place in the assignment where it really makes sense to use tail :: [a] -> [a].
fib :: [Integer]
fib = 0 : 1 : addpairs fib (P.tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: (Ord a) => a -> [a] -> [a]
add n [] = [n]
add n (x : xs) | x < n = x : add n xs
add n (x : xs) | x == n = x : xs
add n (x : xs) | x > n = n : x : xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: (Ord a) => [a] -> [a] -> [a]
union l1 [] = l1
union [] l2 = l2
union (x : xs) (y : ys) | x < y = x : union xs (y : ys)
union (x : xs) (y : ys) | y < x = y : union (x : xs) ys
union (x : xs) (y : ys) | x == y = x : union xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect l1 [] = []
intersect [] l2 = []
intersect (x : xs) (y : ys) | x < y = intersect xs (y : ys)
intersect (x : xs) (y : ys) | y < x = intersect (x : xs) ys
intersect (x : xs) (y : ys) | x == y = x : intersect xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!

-- def powerset(lst):
-- result = [[]]  # start with the empty subset
-- for elem in lst:
--     # for each element, add it to all existing subsets
--     new_subsets = [subset + [elem] for subset in result]
--     result.extend(new_subsets)
-- return result

highUnion :: (Ord a) => [[a]] -> [[a]] -> [[a]]
highUnion l1 [] = l1
highUnion [] l2 = l2
highUnion (x : xs) (y : ys)
  | P.head x < P.head y = x : highUnion xs (y : ys)
  | otherwise = y : highUnion (x : xs) ys

-- powerset :: Ord a => [a] -> [[a]]
-- powerset nl = aux nl [[]]
--   where aux [] m = m
--         aux (x : xs) m = aux xs (highUnion ([add x n | n <- m]) m)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = merge (powerset xs) x
  where
    merge [] _ = []
    merge (ys : yss) x = ys : (x : ys) : merge yss

--- Higher Order Functions
--- ----------------------
-- [[],[0],[1],[0,1]] -- expected
-- [[],[0],[0,1],[1]] -- actual
--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' a = [x + 1 | x <- a]

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' a = sum a
