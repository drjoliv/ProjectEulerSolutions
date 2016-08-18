import qualified EulerUTIL
{-
- Factors are the numbers you multiply together
- to get another number.
- Every number can be written as a product of primes.
-}
problem3 = maximum $ EulerUTIL.primefactors 600851475143
main = print problem3
