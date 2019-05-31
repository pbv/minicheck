--
-- Generators and shrinking for basic types and lists 
--
module Test.Generators where

import           Test.Minicheck(Gen(..), Arbitrary(..))

import           Data.List (sort, tails, inits)
import           Control.Monad
import qualified System.Random as R


-- basic generators
-- | choose a discrete random value uniformly
choose :: R.Random a => (a, a) -> Gen a
choose (lo,hi) = MkGen (fst . R.randomR (lo,hi))


-- | choose a value from a list
elements :: [a] -> Gen a
elements vs = do
  k <- choose (0, length vs-1)
  return (vs!!k)

-- | choose a generator from a list 
oneof :: [Gen a] -> Gen a
oneof gs = do
  k <- choose (0, length gs-1)
  gs!!k


-- | shrink an integral value
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral n
  | n>0 = 0 : [n`div`2 | n>2] ++ [n-2 | n>2] ++ [n-1 | n>1]
  | n<0 = 0 : 1 : [n`div`2 | n<(-2)] ++ [n+2 | n<(-2)] ++ [n+1 | n<(-1)]
shrinkIntegral _ = []



-- | generator for lists of given size
listOf :: Int -> Gen a -> Gen [a]
listOf = replicateM 

-- | generate an ordered list
orderedListOf :: Ord a => Int -> Gen a -> Gen [a]
orderedListOf n g = sort <$> listOf n g

ordered :: Ord a => [a] -> Bool
ordered xs = and $ zipWith (<=) xs (tail xs)

-- | shrink a list
shrinkList :: Arbitrary a => [a] -> [[a]]
shrinkList [] = []
shrinkList xs = [ xs'++ys
                | (xs', _:ys) <-zip (inits xs) (tails xs)
                ] ++
                [ xs'++ (y':ys)
                | (xs', y:ys) <- zip (inits xs) (tails xs),
                  y' <- shrink y
                ]

-- | shrink a list maintaing ordering
shrinkOrderedList :: (Arbitrary a, Ord a) => [a] -> [[a]]
shrinkOrderedList = filter ordered . shrinkList 



--
-- Arbitrary instances 
--

instance Arbitrary Int where
  arbitrary =  choose (minBound, maxBound)
     -- full type range
    --- leads to more interesting "stress test" for shrinking
  shrink = shrinkIntegral 

instance Arbitrary Bool where
  arbitrary = elements [False,True]
  shrink False = []
  shrink True = [False]

  
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = (,) <$> arbitrary <*> arbitrary
  shrink (x,y) = [ (x',y) | x' <- shrink x ] ++
                 [ (x,y') | y' <- shrink y ] 


instance Arbitrary a => Arbitrary [a] where
  arbitrary = listOf 10 arbitrary --  size 10 by default
  shrink = shrinkList
