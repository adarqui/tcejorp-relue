module P02 (
 solve
) where

import Lib

solve :: (Num a, Integral a) => Int -> a


solve n = sum $ filter even $ takeWhile (<n) $ fibFromList [1..]
