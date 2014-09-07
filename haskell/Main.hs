module Main where

import Text.Printf

import Test
import Hoge

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Ord, Enum, Show, Read)

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

data GTree a = Leaf a | Gnode [GTree a]

num_of_leaves :: GTree a -> Integer
num_of_leaves (Leaf _) = 1
num_of_leaves (Gnode x) = foldl (+) 0 $ map num_of_leaves x

main :: IO ()
main = do putStrLn "What's your name?"
          your_name <- getLine
          printf "Hello, %s (%d)!\n" your_name $ lstlen your_name
          printf "%f\n" $ area $ Circle 2.0
          printf "%s\n" $ show $ Spring
          printf "%d\n" $ num_of_leaves $ Gnode [Gnode [Leaf (-1), Gnode [Leaf (-2)], Leaf 0],  Leaf 1, Gnode [Leaf 2, Leaf 3], Leaf 4, Gnode [Leaf 5, Leaf 6, Gnode []]]
          printf "average: %s\n" $ show $ average []
          printf "average: %s\n" $ show $ average [777.777]
          printf "average: %s\n" $ show $ average [1,2,3,4,5]
          test
          run_string_calc_tests
          hoge

lstlen :: [a] -> Int
lstlen lst = lstlen' lst 0
    where
      lstlen' [] n = n
      lstlen' (s:ss) n = lstlen' ss $ n + 1

average :: (Fractional a) => [a] -> a
average [] = 0
average (x:[]) = x
average (x:xs) = average' 0 0 x xs
    where average' s l y [] = (s + y) / (l + 1)
          average' s l y (yy:yys) = average' (s + y) (l + 1) yy yys
