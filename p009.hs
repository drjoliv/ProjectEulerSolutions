import qualified EulerUTIL
{-
 - A Pythagorean triplet is a set  of three natural numbers, a < b < c, for which,
 -                                                             a^2 + b^2 = c^2
 - There exists one Pythagorean triplet for which a + b + c = 1000.
 - Find the Product.
 -
 -}

problem9 = [a * b * c |
                       a <- [1..334],
                       b <- [a+1..498],
                       c <- [b+1.. ceiling . sqrt . fromInteger $ (a^2 + b^2)],
                       (a + b + c) == 1000, a^2 + b^2 == c^2]
main = print problem9
