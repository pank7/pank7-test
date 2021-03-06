module Hoge where

import Text.Printf
import Data.List.Split

import Test.HUnit

hoge :: IO ()
hoge = do printf "Welcome to hoge!\n"
          printf "hoge 1: %d\n" $ string_calc "1,1"
          printf "hoge 2: %d\n" $ string_calc "1,3"

string_calc :: String -> Integer
string_calc expr = sum $ map read $ filter (\x -> x /= "") $ splitOn "," $ filter (\x -> elem x "0123456789,-") expr

prime :: Int -> Bool
prime n | n > 0 = [x | x <- [1..n], n `mod` x == 0] == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- unit tests

simple_test = TestCase (assertEqual "\"1,1\"" 2 (string_calc "1,1"))
another_simple_test = TestCase (assertEqual "\"1,2,3\"" 6 (string_calc "1,2,3"))
big_number_test = TestCase (assertEqual "\"10000000000000000000,1\"" 10000000000000000001 (string_calc "10000000000000000000,1"))
empty_expr_test = TestCase (assertEqual "(empty string)" 0 (string_calc ""))
comma_comma_expr_test = TestCase (assertEqual "1,,1" 2 (string_calc "1,,1"))
negative_number_test = TestCase (assertEqual "1,,-1" 0 (string_calc "1,,-1"))
space_expr_test = TestCase (assertEqual "1, ,-1" 0 (string_calc "1, ,-1"))
invalid_expr_test = TestCase (assertEqual "1,1a,-1" 1 (string_calc "1,1a,-1"))

string_calc_tests = TestList [
                     TestLabel "simple test" simple_test,
                     TestLabel "another simple test" another_simple_test,
                     TestLabel "big number test" big_number_test,
                     TestLabel "empty expr test" empty_expr_test,
                     TestLabel "comma comma expr test" comma_comma_expr_test,
                     TestLabel "negative number test" negative_number_test,
                     TestLabel "space expr test" space_expr_test,
                     TestLabel "invalid expr test" invalid_expr_test
                    ]

run_string_calc_tests = runTestTT string_calc_tests
