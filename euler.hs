import Data.List (delete)
{-@authur Desonte Jolivet
 -@lastModified 1 December 2016
 -}

-- problem 1-----------------------------------------------------
problem_1 = sum [x | x <- [1..999], x `mod` 3 ==0 || x `mod` 5 ==0]

-- problem 2-----------------------------------------------------
problem_2 = sum $ takeWhile (<4000000) [x | x <- fibs, even x]

-- the nth value of Fibonacci sequence
fib :: Integer -> Integer
fib x = fibs !! (fromIntegral  x)

--Infinite list of Fibonacci sequence.
fibs :: [Integer]
fibs = 1 : 2 : [ n | x <-[2..], let n = ((fibs !! (x-1)) + (fibs !! (x-2)))]

-- problem 3-----------------------------------------------------

problem_3 = maximum $ primefactors 600851475143

primefactors :: Integer -> [Integer]
primefactors 0 = [] 
primefactors 1 = [] 
primefactors i = primefactorshelper i primes

--helper method makes interface simpler
primefactorshelper:: Integer -> [Integer] -> [Integer]
primefactorshelper i as@(x:xs)  
     | i == x = [i]
     | x * x > i = [i]
     | i `mod` x == 0 = x : primefactorshelper (i `div` x) as
     | otherwise = primefactorshelper i xs 

--list of primes
primes :: [Integer]
primes = 2 : [ x | x <- [3..], let xs = take 2 $ primefactors x, null (tail xs)]

isPrime :: Integer -> Bool
isPrime x = (length . primefactors) x == 1

-- problem 4 --------------------------------------------------

problem_4 = maximum [x | y <- [100..999], z <- [y..999], let x = y * z, isPalindrome $ show x]

isPalindrome []  = True
isPalindrome [a] = True
isPalindrome (a:xs) = if (a == last xs)
     then isPalindrome (init xs)
     else False 
-- problem 5 --------------------------------------------------
problem_5 = lcm' [1..20]

lcm' :: [Integer] -> Integer
lcm' = product . foldr uniqueUnion [] . map primefactors
     where uniqueUnion :: [Integer] -> [Integer] -> [Integer]
           uniqueUnion [] as = as
           uniqueUnion as [] = as
           uniqueUnion ax@(a:as) ex@(e:es)
                | a `elem` ex = a : uniqueUnion as ( delete a ex )
                | e `elem` ax = e : uniqueUnion es ( delete e ax )
                | otherwise = a : e : uniqueUnion as es


