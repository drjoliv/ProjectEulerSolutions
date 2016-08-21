import Data.List
import qualified EulerUTIL

{-Problem 5 was very interesting. I first did the problem by hand by factoring each number from 1..20
 - into its unique prime factorization. Then I combined each list of primes factors to create a new list.
 - This heuristic approach was very straight forward. After deciding to try and implement this heuristic
 - approach in code i created the function uniqueUnion that combined primefactors of numbers to find the a 
 - number that divided all of them.
 - See EulerUTIL.lcm'
 - It wasnt until later that I realized that I had created my own LCM funciton
 -}
     
problem4 = EulerUTIL.lcm' [1..20]
main = print problem4
 

