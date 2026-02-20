--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake
mytake :: Int -> [a] -> [a]
mytake n _ | n <= 0 = []
mytake _ []         = []
mytake n (x:xs)     = x : mytake (n-1) xs

--- ### mydrop
mydrop :: Int -> [a] -> [a]
mydrop n xs | n <= 0 = xs
mydrop _ []          = []
mydrop n (_:xs)      = mydrop (n-1) xs

--- ### rev
rev :: [a] -> [a]
rev xs = revHelper xs []
  where
    revHelper [] acc     = acc
    revHelper (y:ys) acc = revHelper ys (y:acc)

--- ### app
app :: [a] -> [a] -> [a]
app [] ys     = ys
app (x:xs) ys = x : app xs ys

--- ### inclist
inclist :: Num a => [a] -> [a]
inclist []     = []
inclist (x:xs) = (x + 1) : inclist xs

--- ### sumlist
sumlist :: Num a => [a] -> a
sumlist []     = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _          = []
myzip _ []          = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = sumPairs (myzip xs ys)
  where
    sumPairs []          = []
    sumPairs ((a,b):rest) = (a + b) : sumPairs rest

--- ### ones
ones :: [Integer]
ones = 1 : ones

--- ### nats
nats :: [Integer]
nats = [0..]

--- ### fib
fib :: [Integer]
fib = 0 : 1 : addpairs fib (P.tail fib)

--- Set Theory
--- ----------

--- ### add
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys)
  | x < y     = x : y : ys
  | x == y    = y : ys
  | otherwise = y : add x ys

--- ### union
union :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys)
  | x < y     = x : union xs (y:ys)
  | x == y    = x : union xs ys
  | otherwise = y : union (x:xs) ys

--- ### intersect
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
  | x < y     = intersect xs (y:ys)
  | x > y     = intersect (x:xs) ys
  | otherwise = x : intersect xs ys

--- ### powerset
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union p (P.map (add x) p)
  where p = powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldl (+) 0