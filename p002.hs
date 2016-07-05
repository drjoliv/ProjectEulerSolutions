import qualified EulerUTIL

{- 
 - Solution to Project Euler problem 2
 - author: Desonte Jolivet
 - Problem: By considering the terms in the Fibonacci sequence whose
 - values do not exceed four million, find the sum of the even-valued terms.
 - 
 - Description:
 - [x | x <- EulerUTIL.fibs, even x] list containing even Fibonacci numbers.
 - takeWhile filters all values above four million.
 - Then sum adds all those values still in the list.
 -}
problem_2 = sum $ takeWhile (<4000000) [x | x <- EulerUTIL.fibs, even x]