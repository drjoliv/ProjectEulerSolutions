{- 
 - Solution to Project Euler problem 2
 - author: Desonte Jolivet
 - Problem: By considering the terms in the Fibonacci sequence whose
 - values do not exceed four million, find the sum of the even-valued terms.
 - 
 -}
-- | The fib 'function' returns the nth element in the fibonacci sequence.
-- The fibonacci sequence is the series of numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- The next number in the sequence is found by adding the tqo numbers before it.
-- The Recurance can be defined  a~n~ = (a~n-1~) + (a~n-2~) &#x3BB
fib :: Integer -> Integer
fib x 
	| x == 1 = 1
	| x == 2 = 2
	| x == 3 = 3
	| otherwise = fib (x - 1) + fib (x - 2)

--This solution takes much to long i will have to revisit it and determine why
x = sum (takeWhile (< 4000000) [ans | x <- [1..], let ans = fib x, even ans])
main = print x