{- 
 - Solution to Project Euler problem 6
 - author: Desonte Jolivet
 - Problem: Find the difference between the sum of the
 -squares of the first one hundred natural numbers and the square of the sum.
 - 
 - Description: Calculate sum of the squares and  square of sum of the first 100 
 - natural numbers and return the diffeerence of the two values, abs (sumOfSquares - squareOfSums).
 -}

problem_6 = abs (sumOfSquares - squareOfSums)
  where sumOfSquares = sum [ x*x | x <-[1..100]]
        squareOfSums = square $ sum [1..100]
        square x     = x * x