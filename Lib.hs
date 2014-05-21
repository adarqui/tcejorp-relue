module Lib (
 multiplesOf,
 multiplesOfList,
 fib,
 fibFromList,
 isPrime,
 primeFactorsOf,
 primes,
 sieve,
 primesImposter,
 sieveImposter
) where

import Data.Bits

multiplesOf :: (Enum a, Num a) => a -> [a]
multiplesOf = \x -> [x,x+x..]

multiplesOfList :: (Enum a, Num a) => ([a] -> [b]) -> [a] -> [b]
multiplesOfList = \(tw) -> \l -> concatMap (tw . multiplesOf) l

fib :: (Num a, Enum a) => a -> a
fib n = snd $ foldl (\(x,y) z -> (y,x+y)) (0,1) [2..n]

fibFromList :: (Num b, Num a, Enum a) => [a] -> [a]
fibFromList l = map fib l

isPrime :: Int -> Bool
isPrime n = case (n > 1) of
 True -> not $ any (== False) [ n `mod` x /= 0 | x <- [2..n-1], x ^ 2 <= n ]
 False -> False

primeFactorsOf :: Int -> [Int]
primeFactorsOf n = [n]

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]

sieve (p:ps) xs = h ++ sieve ps [ x | x <- t, rem x p /= 0]
 where
  (h, ~(_:t)) = span(< p*p) xs

primesImposter :: (Num a, Enum a, Integral a) => [a]
primesImposter = sieveImposter [2..]

sieveImposter :: (Num a, Enum a, Integral a) => [a] -> [a]
sieveImposter (p:xs) = p : sieveImposter [ x | x <- xs, (x < p*p) || (x `mod` p > 0) ]

{-
isPalNum :: (Bits a, Fractional a) => a -> Int -> Bool
isPalNum n s = (.&.) a b == ((2 ^ s) - 1 - (2 ^ (s / 2) + 1))
 where
  a = n
  b = shiftL a 2
  -}

bitString :: Int -> Int -> [Int]
bitString x y = map numFalse $ tbit x 0 y []
 where
  tbit i m n l = case (m == n) of
   True -> l
   False -> tbit i (m+1) n (l++[testBit i m])

numFalse :: Bool -> Int
numFalse True = 1
numFalse False = 0

