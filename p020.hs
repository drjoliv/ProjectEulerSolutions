{-
 - Fractorial digit sum
 - author: Desonte Jolivet
 - Problem: Find the sum of the digits in the number 100!
 -}

import Data.Char

fractorial 1 = 1
fractorial n = n * fractorial ( n -1 )

--Find 100! convert the number to a string of chars map each char to an int
-- sum list of ints
problem20 = sum $ map digitToInt . show . fractorial $ 100
main = print problem20
