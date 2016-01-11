{- 
 - Solution to Project Euler problem 2
 - author: Desonte Jolivet
 - Problem: By considering the terms in the Fibonacci sequence whose
 - values do not exceed four million, find the sum of the even-valued terms.
 -}

fib :: Integer -> Integer
fib x 
    | x < 0 = error 'Not Defined'
	| x == 1 = 1
	| x == 2 = 2
	| x == 3 = 3
	| otherwise = fib (x - 1) + fib (x - 2)

--This solution takes much to long i will have to revisit it and determine why
x = sum (takeWhile (< 4000000) [ans | x <- [1..], let ans = fib x, even ans])