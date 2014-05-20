module P01 (
 multiplesOf,
 multiplesOfList,
 solveList
) where

import Data.List
import Lib

solveList :: (Ord a, Num a, Enum a) => a -> [a] -> a


solveList n l = sum $ nub $ multiplesOfList (takeWhile (<n)) l
