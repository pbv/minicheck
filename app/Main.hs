module Main where

import Test.Minicheck


prop_even :: Int -> Int -> Bool
prop_even x y = (x>10 && y>10) `implies` even (x+y)

implies x y = not x || y

prop_other
  = forAllShrink (choose (0,10)) shrink $ \x ->
   forAllShrink (choose (0,10)) shrink $ \y ->
      even (x + y :: Int)

-- try: "checkTree prop_other"


main :: IO ()
main = do
  check prop_even
