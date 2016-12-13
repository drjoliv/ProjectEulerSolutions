import Data.List (delete, sort, elemIndex, group, findIndex)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, difference)
import Data.Function ((&))
import Data.Maybe (fromJust, catMaybes)
import Data.Char (digitToInt, intToDigit)
import Control.Monad.State
import Numeric (showIntAtBase)
import qualified Data.Set as Set
import qualified Data.Map as M

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

----isPrime :: Integer -> Bool
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

-- problem 6 -------------------------------------------------

problem_6 = abs (sumOfSquares 100 - squareOfSums 100)

sumOfSquares n = sum [ x*x | x <-[1..n]]
squareOfSums n = square $ sum [1..n]
square       x = x * x

-- problem 7 -------------------------------------------------

problem_7 = primes !! 10000

-- problem 8 -------------------------------------------------

problem_8 = evalState state_largest_product (_1000_digit, 0, 13)

_1000_digit = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

state_largest_product :: State (String, Int, Int) Int
state_largest_product = get >>= \(x, y, z) -> let x' = drop 1 x
                                                  s  = productString (take z x)
                                               in if length x < z
                                                  then return $ y 
                                                  else if s > y
                                                       then put (x', s, z) >> state_largest_product
                                                       else put (x', y, z) >> state_largest_product

productString :: String -> Int 
productString s = product $ map digitToInt s 

-- problem 9 -------------------------------------------------
problem_9 = [a * b * c |
                       a <- [1..334],
                       b <- [a+1..498],
                       c <- [b+1.. ceiling . sqrt . fromInteger $ (a^2 + b^2)],
                       (a + b + c) == 1000, a^2 + b^2 == c^2]

-- problem 10 ------------------------------------------------

problem_10 = sum $  takeWhile (<2000000) primes

-- problem 11 ------------------------------------------------

problem_11_nums = readFile "p011input.txt" >>= return
                             . chunksOf 20
                             . (map (read :: String -> Int))
                             . words

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

problem_13 = readFile "p013input.txt" >>= (\s -> print $ (take 10 . show . sum) $ read <$> lines s)

-- problem 16 -------------------------------------------------
problem_16 = sum $ map digitToInt $  show $ 2 ^ 1000

-- problem 20 -------------------------------------------------
fractorial 1 = 1
fractorial n = n * fractorial ( n -1 )

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

problem_22 = readFile "names_p022.txt" >>=
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
                                              _        -> [Nothing]
           n :: [Int]
           n            =  [1..28123] >>= isAbundant & catMaybes
           n'           = [ x + y | x <- n, y <- n, y >= x ]

-- problem 25 --------------------------------------------------

problem_25 = fromJust (findIndex (\x -> (length . show) x == 1000) fibs) + 2 

-- problem 34 --------------------------------------------------

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

