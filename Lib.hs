module Lib (
 multiplesOf,
 multiplesOfList,
 fib,
 fibFromList
) where


multiplesOf :: (Enum a, Num a) => a -> [a]
multiplesOfList :: (Enum a, Num a) => ([a] -> [b]) -> [a] -> [b]
fib :: (Num a, Enum a) => a -> a
fibFromList :: (Num b, Num a, Enum a) => [a] -> [a]


multiplesOf = \x -> [x,x+x..]

multiplesOfList = \(tw) -> \l -> concatMap (tw . multiplesOf) l

fib n = snd $ foldl (\(x,y) z -> (y,x+y)) (0,1) [2..n]

fibFromList l = map fib l
