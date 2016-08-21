{-
 - author : Desonte 'drjoliv' Jolivet
 - description : Util functions that will probaly come in handy in many euler problems
 -} 
module EulerUTIL
( fib
, fibs
, primes
, primefactors
, lcm
) where

import Data.List

-- | The fib 'function' returns the nth element in the fibonacci sequence.
-- The fibonacci sequence is the series of numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- The next number in the sequence is found by adding the two numbers before it.
-- The Recurance can be defined  FIBn = FIB(n-1) + FIB(n-2)
fib :: Integer -> Integer
fib x = fibs !! (fromIntegral  x)

--Infinite list of Fibonacci sequence.
fibs :: [Integer]
fibs = 1 : 2 : [ n | x <-[2..], let n = ((fibs !! (x-1)) + (fibs !! (x-2)))]

{-a prime is any number that is only divisible by it self and one
 - The fundamental therom of arithmetic "describes that every integer greater than
 - is either prime or a product of primes.
--Returns list of prime factors
-}
primefactors :: Integer -> [Integer]
primefactors i = primefactorshelper i primes    

--helper method makes interface simpler
primefactorshelper:: Integer -> [Integer] -> [Integer]
primefactorshelper i as@(x:xs)  
     | i == x = [i]
     | x * x > i = [i]
     | i `mod` x == 0 = x : primefactorshelper (i `div` x) as
     | otherwise = primefactorshelper i xs 

--list of primes
primes :: [Integer]
primes = 2 : [ x | x <- [3..], let xs = take 2 $ primefactors x, null (tail xs)]

--Finds the least common multiple of a list of numbers
lcm' :: [Integer] -> Integer
lcm' = product . foldr uniqueUnion [] . map EulerUTIL.primefactors
     where uniqueUnion :: [Integer] -> [Integer] -> [Integer]
           uniqueUnion [] as = as
           uniqueUnion as [] = as
           uniqueUnion ax@(a:as) ex@(e:es)
                | a `elem` ex = a : uniqueUnion as ( delete a ex )
                | e `elem` ax = e : uniqueUnion es ( delete e ax )
                | otherwise = a : e : uniqueUnion as es
