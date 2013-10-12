module Fib where

import Prelude
import Data.List
import Data.Bits

naive_fib :: Int -> Integer
naive_fib n
          | n < 0 = -1
          | n == 0 = 0
          | n == 1 = 1
          | otherwise = (naive_fib (n - 1)) + (naive_fib (n - 2))

fib :: Int -> Integer
fib n
    | n < 0 = -1
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = tr_fib 0 1 n

tr_fib :: Integer -> Integer -> Int -> Integer
tr_fib ff f n
       | n == 0 = ff
       | otherwise = tr_fib f (ff + f) (n - 1)

fast_fib :: Int -> Integer
fast_fib n
         | n < 0 = -1
         | n == 0 = 0
         | otherwise = fst (fast_fib' (n - 1))

fast_fib' :: Int -> (Integer, Integer)
fast_fib' 0 = (1, 1)
fast_fib' 1 = (1, 2)
fast_fib' n
          | even n    = (a*a + b*b, c*c - a*a)
          | otherwise = (c*c - a*a, b*b + c*c)
          where
            (a, b) = fast_fib' (n `div` 2 - 1)
            c = a + b

faster_fib :: Int -> Integer
faster_fib n = snd . foldl faster_fib' (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
               where
                 unfoldl f x = case f x of
                   Nothing     -> []
                   Just (u, v) -> unfoldl f v ++ [u]
                 divs 0 = Nothing
                 divs k = Just (uncurry (flip (,)) (k `divMod` 2))
                 faster_fib' (f, g) p
                             | p         = (f*(f+2*g), f^2 + g^2)
                             | otherwise = (f^2+g^2,   g*(2*f-g))
 
fastest_fib :: Int -> Integer
fastest_fib n = snd . foldl' fastest_fib' (1, 0) . dropWhile not $
                [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
                where
                  fastest_fib' (f, g) p
                               | p         = (f*(f+2*g), ss)
                               | otherwise = (ss, g*(2*f-g))
                               where ss = f*f+g*g
