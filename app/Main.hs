--
-- Example properties with Minicheck 
--
module Main where

import Test.Minicheck
import Test.Generators   --  for lists

-- Example 1
-- both these properties are false
prop_even :: Int -> Int -> Bool
prop_even x y = 
    (x>10 && y>10) `implies` even (x+y)

implies x y = not x || y

prop_sum :: Prop
prop_sum =
  forAllShrink (choose (0, 100::Int)) shrink $ \x ->
  forAllShrink (choose (0, 100::Int)) shrink $ \y ->
      (x < 10 && y<10) `implies` (x + y < 10)


-- Example 2
-- | broken implementation of list insertion
--
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = y : insert y ys
                | otherwise = x : ys

-- partial corretness property:
-- for all x, ys: ordered xs => ordered (insert x ys)

-- Version 1: expressed in curried form
prop_insert1 :: Prop
prop_insert1
  = forAllShrink numbers shrink $ \x ->
    forAllShrink (orderedListOf 10 numbers) shrinkOrderedList $ \ys ->
           ordered (insert x ys)

-- Version 2: expressed in uncurried form
-- this leads to better counter-examples than above!
prop_insert2 :: Prop
prop_insert2
  = forAllShrink ((,) <$> numbers <*> orderedListOf 10 numbers)
    shrinkPair $ \(x,ys) ->  ordered (insert x ys)

shrinkPair (x, ys) = [ (x', ys) | x'<-shrink x ] ++
                     [ (x, ys') | ys'<-shrinkOrderedList ys ]

numbers :: Gen Int
numbers = choose (0,100)



-- Example 3
-- for all x, y: x <= y
-- from x=3, y=2 obtain the simplest counter-example

-- version 1: curried
prop_lte1
  = forAllShrink (return 3 :: Gen Int) shrink $ \x ->
    forAllShrink (return 2)  shrink $ \y -> x <= y

-- version 2: uncurried
prop_lte2
  = forAllShrink (return (3,2) :: Gen (Int,Int)) shrink $ \(x,y) ->
    x <= y


main :: IO ()
main = mapM_ runCheck
       [ ("prop_even", check prop_even)
       , ("prop_sum", check prop_sum)
       , ("prop_insert1", check prop_insert1)
       , ("prop_insert2", check prop_insert2)
       , ("prop_lte1", check prop_lte1)
       , ("prop_lte2", check prop_lte2)  
       ]
  where
    runCheck (name, action) = do
      putStrLn name 
      action
      putStrLn (replicate 40 '-')
      
