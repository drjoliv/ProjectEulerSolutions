{- 
 - Solution to Project Euler problem 1
 - author: Desonte Jolivet
 - Multiples of 3 and 5
 - Problem: Find the sum of all the multiples of 3 or 5 below 1000.
 - Create list comprehension drawing from a list of natural numbers from 1 to 999
 - Filter the list of 1 to 999 with the predicate x `mod` 3 ==0 || x `mod` 5 ==0
 -}

problem_1 = sum [x | x <- [1..999], x `mod` 3 ==0 || x `mod` 5 ==0]