{-# LANGUAGE UnicodeSyntax #-}
{-|
 - Module      : Euler
 - Description : Short description
 - Copyright   : (c) Some Guy, 2013 Someone Else, 2014
 - License     : GPL-3
 - Maintainer  : jolivetdesonte@yahoo.com
 - Stability   : experimental
 - Portability : POSIX
 -
 - Here is a longer description of this module, containing some
 - commentary with @some markup@.
 -}
module Euler where

import Data.List (delete, sort, elemIndex, group, findIndex, transpose)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, difference)
import Data.Function ((&))
import Data.Maybe (fromJust, catMaybes)
import Data.Char (digitToInt, intToDigit)
import Control.Monad.State
import Numeric (showIntAtBase)
import qualified Data.Set as Set
import qualified Data.Map as M


-- | problem 1
problem_1 = sum [x | x <- [1..999], x `mod` 3 ==0 || x `mod` 5 ==0]

-- | problem 2
problem_2 = sum $ takeWhile (<4000000) [x | x <- fibs, even x]

-- |The 'fib' function takes an Integer and returns the 
-- nth element in the fibonacci sequence.
fib :: Integer -- ^ The fibonnaci number to obtain.
    -> Integer -- ^ The 'nth' fibonacci number
fib x = fibs !! (fromIntegral  x)

-- |Infinite list of Fibonacci sequence.
fibs :: [Integer]
fibs = 1 : 2 : [ n | x <-[2..], let n = ((fibs !! (x-1)) + (fibs !! (x-2)))]

-- | problem 3
problem_3 = maximum $ primefactors 600851475143

-- | The 'primefactors' function finds list of primefactors of a number. 
-- nth element in the fibonacci sequence.
primefactors :: Integer   -- ^ A 'Interger' to find primefactors of..
             -> [Integer] -- ^ A '[Interger]' containing list of primes
primefactors 0 = []
primefactors 1 = []
primefactors i = primefactorshelper i primes
    where primefactors' :: Integer -> [Integer] -> [Integer]
          primefactors' i as@(x:xs)  
            | i == x = [i]
            | x * x > i = [i]
            | i `mod` x == 0 = x : primefactors' (i `div` x) as
            | otherwise = primefactors' i xs


-- |'primes' is an infinite list of primes.
primes :: [Integer]
primes = 2 : [ x | x <- [3..], let xs = take 2 $ primefactors x, null (tail xs)]

-- | The 'isPrime' function check if an Integer is prime.
isPrime :: Integer -- ^ An 'Integer' whose primality to check.
        -> Bool    -- ^ 'True' if number is prime otherwise 'False'.
isPrime x = (length . primefactors) x == 1

-- | problem 4
problem_4 = maximum [x | y <- [100..999], z <- [y..999], let x = y * z, isPalindrome $ show x]

-- | The 'isPalindrome' checks if a List is a plaindrome.
isPalindrome :: Eq a => [a] -- ^ a 'List' of 'a' values
                     -> Bool
isPalindrome []  = True
isPalindrome [a] = True
isPalindrome (a:xs) = if (a == last xs)
     then isPalindrome (init xs)
     else False

-- | problem 5
problem_5 = lcm' [1..20]

-- | This function returns the least common multiple of list of Integer
lcm' :: [Integer] -- ^ [Integer] we would like to find the lease common multiple of
     -> Integer   -- ^ Least Commom Multiple
lcm' = product . foldr uniqueUnion [] . map primefactors
     where uniqueUnion :: [Integer] -> [Integer] -> [Integer]
           uniqueUnion [] as = as
           uniqueUnion as [] = as
           uniqueUnion ax@(a:as) ex@(e:es)
                | a `elem` ex = a : uniqueUnion as ( delete a ex )
                | e `elem` ax = e : uniqueUnion es ( delete e ax )
                | otherwise = a : e : uniqueUnion as es

-- | problem 6
problem_6 = abs (sumOfSquares 100 - squareOfSums 100)

sumOfSquares n = sum [ x*x | x <-[1..n]]
squareOfSums n = square $ sum [1..n]
square       x = x * x

-- | problem 7
problem_7 = primes !! 10000

-- | problem 8
problem_8 = largestProduct 13 $ stringToIntList _1000_digit

_1000_digit = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"


state_largest_product :: State (String, Int, Int) Int
state_largest_product = get >>= \(x, y, z) -> let x' = drop 1 x
                                                  s  = productString (take z x)
                                               in if length x < z
                                                  then return $ y 
                                                  else if s > y
                                                       then put (x', s, z) >> state_largest_product
                                                       else put (x', y, z) >> state_largest_product

stringToIntList :: String -> [Int]
stringToIntList = map digitToInt

productString :: String -> Int 
productString s = product $ map digitToInt s 

-- | problem 9
problem_9 = [a * b * c |
                       a <- [1..334],
                       b <- [a+1..498],
                       c <- [b+1.. ceiling . sqrt . fromInteger $ (a^2 + b^2)],
                       (a + b + c) == 1000, a^2 + b^2 == c^2]

-- | problem 10
problem_10 = sum $  takeWhile (<2000000) primes

-- problem 11 ------------------------------------------------
{-
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
-}


--problem_11 =
--    problem_11_nums                                       >>= \xs ->
--    return $ maximum $ map largestProduct' xs             >>= \x1 ->
--    return $ maximum $ map largestProduct' (transpose xs) >>= \x2 ->
--    return $ maximum $ map largestProduct' (diagLeft xs)  >>= \x3 ->
--    return $ maximum $ map largestProduct' (diagRight xs) >>= \x4 ->
--    return $ maximum [x1, x2, x3, x4]
--        where largestProduct' = largestProduct 4
--              diagLeft        = transpose
--              diagRight       = transpose

problem_11_nums file = readFile file >>= return
                             . (map (read :: String -> Int))
                             . words

--left = problem_11_nums >>= chunksOf 20 

largestProduct :: Int -> [Int] -> Int 
largestProduct i is = maximum $ evalState replicateProductOf is
    where splitThis n xs        = (take n xs, drop 1 xs)
          productOf             = (fmap product) . state . splitThis
          replicateProductOf    = replicateM (length is) (productOf i)

-- problem 12 ------------------------------------------------
trianglenumbers = scanl1 (+) [1..]

problem_12 = take 1 [x |
                       x <- trianglenumbers,
                       sqrt (fromInteger  x) > 250,
                       (Set.size (divisors x 1 (Set.fromList [1]))) > 500]
    where divisors :: Integer -> Integer -> Set Integer -> Set Integer 
          divisors i a nums
           | a * a > i = nums 
           | i `mod` a == 0  = divisors i (a+1) $ Set.insert (i `div` a) $ Set.insert a nums 
           | otherwise = divisors i (a+1) nums

-- problem 13 -------------------------------------------------
problem_13 file = readFile file >>= (\s -> print $ (take 10 . show . sum) $ read <$> lines s)

-- problem 16 -------------------------------------------------
problem_16 = sum $ map digitToInt $  show $ 2 ^ 1000

fractorial 1 = 1
fractorial n = n * fractorial ( n -1 )

-- problem 20 -------------------------------------------------
problem_20 = sum $ map digitToInt . show . fractorial $ 100

-- problem 21 -------------------------------------------------
divisors x               = map product $ sequence [take (k+1) $ iterate (p*) 1 | (p,k) <- primepowers x]
     where primepowers n = [(head x, length x) | x <- group $ primefactors n]

d :: Int -> Int
d n = fromInteger . sum . init . divisors . toInteger $ n

d'' a = let b = d a
            c = d b
        in (c == a && a /= b, b)

insert :: Int -> Bool -> State (M.Map Int Bool) ()
insert a b = state $ \_map -> ((),M.insert a b _map)

present :: Int -> State (M.Map Int Bool) Bool
present a = state $ \_map ->  (a `M.member` _map, _map)

amicableNumber 0 = get
amicableNumber n = present n >>= \a -> if a
                                       then amicableNumber (n-1) 
                                       else case d'' n of
                                                 (True, x) -> insert x True >> insert n True >> amicableNumber (n-1)
                                                 _         -> amicableNumber (n-1)

problem_21 =  sum . (map (\(x, y) -> x)) $ M.toList $ execState (amicableNumber 10000) M.empty

-- problem 22 -------------------------------------------------
problem_22 file = readFile file >>=
       (print  . sum
               . zipWith (*) [1..]
               . (map (sum . (map position)))
               . sort
               . (read :: String -> [String]))
     where position :: Char -> Int
           position c = (fromJust $ elemIndex c ['A'..'Z']) + 1

-- problem 23 --------------------------------------------------

data Abundance = Perfect | Abundant | Deficient deriving (Show)

abundance n
     | n' > n = Abundant
     | n' < n = Deficient
     | otherwise = Perfect
          where n' = sum $ filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

problem_23 = sum $ difference (fromList [1..28123]) (fromList n')  
     where isAbundant x = case abundance x of Abundant -> [Just x]
           n :: [Int]
           n            =  [1..28123] >>= isAbundant & catMaybes
           n'           =  [ x + y | x <- n, y <- n, y >= x ]

-- | problem 25
problem_25 = fromJust (findIndex (\x -> (length . show) x == 1000) fibs) + 2 


digitFactorial :: Int -> Int
digitFactorial x = sum $ (map factorial') (map digitToInt (show x))
     where factorial' 0 = 1
           factorial' 1 = 1
           factorial' 2 = 2
           factorial' 3 = 6
           factorial' 4 = 24
           factorial' 5 = 120
           factorial' 6 = 720
           factorial' 7 = 5040
           factorial' 8 = 40320
           factorial' 9 = 362880

isDigitFactorial x = x == digitFactorial x

-- problem 34 --------------------------------------------------
problem_34 =  sum [ x | x <- [3..1000000], isDigitFactorial x]

-- problem 35 --------------------------------------------------
-- problem 36 --------------------------------------------------

isDoublePalindrome x = isPalindrome x && (isPalindrome . toBinary . read) x
     where toBinary = toBase 2

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intToDigit num ""

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = (flip (<$>))

($$) = flip ($)

problem_36 = (sum . (map read)) $ (show <$> [1..1000000]) $$ (filter isDoublePalindrome)

-- problem 37 --------------------------------------------------

isTruncatablePrime p = all isPrime $ nub' p
     where nub' x = (Set.toList . Set.fromList) (truncateRight x  ++ truncateLeft x)

truncateRight :: Integer -> [Integer]
truncateRight s = let s' = show s in (map read) $ map (`take` s') [1.. length s']

truncateLeft :: Integer -> [Integer]
truncateLeft s = let s' = show s in (map read) $ map (`drop` s') [0.. length s' - 1]

problem_37 = sum $ (take 11) (filter isTruncatablePrime (drop 4 primes))

-- problem 43 -------------------------------------------------

--pandigitals = [ x | x <- [1406357289..9999999999], isPandigital x]

isPandigital :: Int -> Bool
isPandigital x =  x `mod` 9 == 0 && sumDigits x == 45 && 
    where digitsSum = sum [1..9]
          isPandigital' x = 
sumDigits x = if x == 0 then 0 else (x `mod` 10 ) + sumDigits (x `div` 10)

main = problem_11_nums >>= print
