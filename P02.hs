module P02 (
 solve
) where

import Lib

solve :: (Num a, Integral a) => a


solve = sum $ filter even $ takeWhile (<4000000) $ fibFromList [1..]
