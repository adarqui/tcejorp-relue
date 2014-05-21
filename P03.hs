module P03 (
 solve
) where

import Lib

{-
 pfac of 13195 = {5,7,13,29}
 largest pfac of 600851475143?
-}

solve n = maximum $ filter (\x -> n `mod` x == 0) $ takeWhile (\x -> x*x < n) primes
