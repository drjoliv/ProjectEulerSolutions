{- 
 - Solution to Project Euler problem 2
 - author: Desonte Jolivet
 - Problem: By considering the terms in the Fibonacci sequence whose
 - values do not exceed four million, find the sum of the even-valued terms.
 - 
 -}
-- | The fib 'function' returns the nth element in the fibonacci sequence.
-- The fibonacci sequence is the series of numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- The next number in the sequence is found by adding the two numbers before it.
-- The Recurance can be defined  FIBn = FIB(n-1) + FIB(n-2)
fib :: Integer -> Integer
fib x 
    | x == 1 = 0
    | x == 2 = 1
    | otherwise = fib (x - 1) + fib (x - 2)


--Infinite list of Fibonacci sequence.
fibs :: [Integer]
fibs = 1 : 2 : [ n | x <-[2..], let n = ((fibs !! (x-1)) + (fibs !! (x-2)))]
better = sum $ takeWhile (<4000000) [x | x <- fibs, even x]
main = print better
