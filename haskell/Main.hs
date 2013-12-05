module Main where

import Text.Printf

main :: IO ()
main = do putStrLn "What's your name?"
          your_name <- getLine
          printf "Hello, %s (%d)!\n" your_name $ lstlen your_name

lstlen :: [a] -> Int
lstlen lst = lstlen' lst 0
    where
      lstlen' [] n = n
      lstlen' (s:ss) n = lstlen' ss $ n + 1
