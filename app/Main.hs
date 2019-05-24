module Main where

import Test.Minicheck


prop_even :: Int -> Int -> Bool
prop_even x y = (x>100 && y>100) `implies` even (x+y)

implies x y = not x || y


main :: IO ()
main = check prop_even 
