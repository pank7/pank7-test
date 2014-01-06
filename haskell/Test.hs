module Test where

import Text.Printf

test :: IO ()
test = do printf "%s\n" $ show [2,4,5,6,6,4,3,5,3,3,5,67,0]
          printf "%s\n" $ show $ partition [2,4,5,6,6,4,3,5,3,3,5,67,0]
          printf "%s\n" $ show [2,1,3]
          printf "%s\n" $ show $ partition [2,1,3]
          putStrLn $ show [4,2,1,3]
          putStrLn $ show $ partition [4,2,1,3]
          putStrLn $ show [0,4,2,1,3]
          putStrLn $ show $ partition [0,4,2,1,3]
          putStrLn $ show $ map chain [1..10]

partition :: (Ord a) => [a] -> [a]
partition [] = []
partition [x] = [x]
partition (x:xs) = partition' x xs [] []
    where partition' x [] l r = l ++ [x] ++ r
          partition' x (y:ys) l r
              | x > y = partition' x ys (l ++ [y]) r
              | otherwise = partition' x ys l (r ++ [y])

chain :: (Integral a) => a -> [a]  
chain n = chain' n []
    where chain' 1 c = reverse (1:c)
          chain' n c
              | even n = chain' (n `div` 2) (n:c)
              | odd n = chain' (n * 3 + 1) (n:c)
