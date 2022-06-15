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

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n [] = []
mytake n (x:xs) 
            | n >= 0 = x : mytake (n-1) xs
            | otherwise = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop n [] = []
mydrop n (x:xs) 
            | n >= 0 = mydrop (n-1) xs
            | otherwise  = x:xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev xs = rev_ xs []
        where rev_ :: [a] -> [a] -> [a]
              rev_  [] xs = xs
              rev_ (x:xs) xss = rev_ xs (x:xss)


--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] xs = xs
app xs [] = xs
app (x:xs) x2 = x : app xs x2

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1):inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] xs = []
myzip xs [] = []
myzip (x1:xs1) (x2:xs2) = (x1,x2) : myzip xs1 xs2

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs [] = []
addpairs [] xs = []
addpairs x1 x2 = [zipadd m | m <- myzip x1 x2]
            where zipadd :: (Num a) => (a,a) -> a
                  zipadd (x,y) = x+y

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones


--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0 : [x+1 | x <- nats]


--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs) 
            |  x < n = x : add n xs
            |  x > n = n : x : xs
            |  otherwise = x:xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] xs = xs
union xs [] = xs
union (x1:xs1) (x2:xs2) 
                | x1 < x2 = x1 : union xs1 (x2:xs2)
                | x1 > x2 = x2 : union (x1:xs1) xs2
                | otherwise = x1 : union xs1 xs2

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] xs = []
intersect xs [] = []
intersect (x1:xs1) (x2:xs2)
                | x1 < x2 = intersect xs1 (x2:xs2)
                | x1 > x2 = intersect (x1:xs1) xs2
                | otherwise = x1 : intersect xs1 xs2

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (powerset xs) ([x:m|m<-powerset xs])

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' [] = []
inclist' xs = P.map (+ 1) xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' [] = 0
sumlist' xs = P.foldr (+) 0 xs

-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)