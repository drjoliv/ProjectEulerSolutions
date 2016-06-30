module EulerUTIL
( fib
, fibs
) where


-- | The fib 'function' returns the nth element in the fibonacci sequence.
-- The fibonacci sequence is the series of numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- The next number in the sequence is found by adding the two numbers before it.
-- The Recurance can be defined  FIBn = FIB(n-1) + FIB(n-2)
fib :: Integer -> Integer
fib x = fibs !! x


--Infinite list of Fibonacci sequence.
fibs :: [Integer]
fibs = 1 : 2 : [ n | x <-[2..], let n = ((fibs !! (x-1)) + (fibs !! (x-2)))]
