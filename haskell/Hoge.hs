module Hoge where

import Text.Printf
import Data.List.Split

import Test.HUnit

hoge :: IO ()
hoge = do printf "Welcome to hoge!\n"
          printf "hoge 1: %d\n" $ string_calc "1,1"
          printf "hoge 2: %d\n" $ string_calc "1,3"

string_calc :: String -> Integer
string_calc expr = sum $ map read $ filter (\x -> x /= "") $ splitOn "," expr

-- unit tests

simple_test = TestCase (assertEqual "\"1,1\"" 2 (string_calc "1,1"))
another_simple_test = TestCase (assertEqual "\"1,2,3\"" 6 (string_calc "1,2,3"))
big_number_test = TestCase (assertEqual "\"10000000000000000000,1\"" 10000000000000000001 (string_calc "10000000000000000000,1"))
empty_expr_test = TestCase (assertEqual "(empty string)" 0 (string_calc ""))
comma_comma_expr_test = TestCase (assertEqual "1,,1" 2 (string_calc "1,,1"))
negative_number_test = TestCase (assertEqual "1,,-1" 0 (string_calc "1,,-1"))

string_calc_tests = TestList [
                     TestLabel "simple test" simple_test,
                     TestLabel "another simple test" another_simple_test,
                     TestLabel "big number test" big_number_test,
                     TestLabel "empty expr test" empty_expr_test,
                     TestLabel "comma comma expr test" comma_comma_expr_test,
                     TestLabel "negative number test" negative_number_test
                    ]

run_string_calc_tests = runTestTT string_calc_tests
