{- 
 - Solution to Project Euler problem 4
 - author: Desonte Jolivet
 - Problem: A palindromic number reads the same both ways.
 - The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 - Find the largest palindrome made from the product of two 3-digit numbers.
 - 
 - Description: Generates every pair of number between 100 and 999, then checks
 - if each pairs product is a palindrome if so addes it to the list. Finally returns 
 - the largest palindrome in the list.
 -}
problem_4 = maximum [x | y <- [100..999], z <- [y..999], let x = y * z, isPalindrome $ show x]
   where isPalindrome []  = True
         isPalindrome [a] = True
         isPalindrome (a:xs) = if (a == last xs)
           then isPalindrome (init xs)
           else False 
