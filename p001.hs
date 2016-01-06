{- 
 - Solution to Project Euler problem 1
 - author: Desonte Jolivet
 - Problem: Find the sum of all the multiples of 3 or 5 below 1000.
 -}

x = sum [x | x <- [1..999], x `mod` 3 ==0 || x `mod` 5 ==0]
main = print x