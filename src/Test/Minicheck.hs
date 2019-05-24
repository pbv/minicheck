{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE BangPatterns  #-}

module Test.Minicheck where

import qualified System.Random as R
import           Data.Tree       --  multiway trees a.k.a. rose trees 
import           Control.Monad
import           Text.Printf

--
-- generators
--
newtype Gen a = MkGen { runGen :: R.StdGen -> a }
              deriving (Functor, Applicative)

-- | NB: this is not just the reader monad for (StdGen ->)
-- because the random generator is split between computations
instance Monad Gen where
  m >>= f = MkGen (\s -> let (s', s'') = R.split s
                         in runGen (f (runGen m s')) s'')

choose :: R.Random a => (a, a) -> Gen a
choose (lo,hi) = MkGen (fst . R.randomR (lo,hi))

elements :: [a] -> Gen a
elements vs = do
  k <- choose (0, length vs-1)
  return (vs!!k)

oneof :: [Gen a] -> Gen a
oneof gs = do
  k <- choose (0, length gs-1)
  gs!!k


-- | sample a generator
sample :: Gen a -> IO a
sample gen = do
  (s,s') <- R.split <$> R.getStdGen
  R.setStdGen s'
  return (runGen gen s)


--
-- type class for generation & shrinking 
-- 
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink = const []


instance Arbitrary Int where
  arbitrary =  choose (minBound, maxBound)
  shrink  = shrinkIntegral 

instance Arbitrary Bool where
  arbitrary = elements [False,True]
  shrink False = []
  shrink True = [False]


-- | NB: this will generate a *very large* shrink lists;
-- must be used with some search limiting criteria
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral 0 = []
shrinkIntegral n
  | n>0 = 0 : [ n`div`2 | n>2 ] ++ [n-1, n-2.. 1]
  | n<0 = 0 : 1 : [ n`div`2 | n>2 ] ++ [-1, -2..n+1]                 

  
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = (,) <$> arbitrary <*> arbitrary
  shrink (x,y) = [ (x',y) | x' <- shrink x ] ++
                 [ (x,y') | y' <- shrink y ] 

--
-- properties and testing
--
data Result = MkResult { ok :: Bool
                       , testCase :: [String]
                       }
            deriving Show

newtype Prop = MkProp { unProp :: Gen (Tree Result) }

liftBool :: Bool -> Result
liftBool b = MkResult {ok=b, testCase=[]}

mapResult :: Testable prop => (Result -> Result) -> prop -> Prop
mapResult f = MkProp . fmap (fmap f). unProp . property 

-- | Adds the given string to the counterexample 
counterexample :: Testable prop => String -> prop -> Prop
counterexample s = mapResult (\r -> r{ testCase = s:testCase r }) 


class Testable a where
  property :: a -> Prop

instance Testable Prop where
  property p = p

instance Testable Bool where
  property b =
    MkProp $ return $ Node { rootLabel = liftBool b, subForest=[] }


instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property pf = forAllShrink arbitrary shrink pf

forAllShrink :: (Show a, Testable prop) =>
                Gen a -> (a -> [a]) -> (a -> prop) -> Prop
forAllShrink gen shrinker pf = MkProp $ do
  a <- gen
  unProp $ shrinking shrinker a pf

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Prop
forAll gen = forAllShrink gen (const []) 


shrinking :: (Testable prop, Show a) => (a -> [a]) -> a -> (a -> prop) -> Prop
shrinking shrinker x0 pf 
  = MkProp $ fmap joinTree (promote (props x0))
 where
   props x = Node { rootLabel =
                      unProp $ property $ counterexample (show x) $ pf x
                  , subForest = [ props x' | x' <- shrinker x ]
                  }


maxTests, maxShrinks :: Int
maxTests = 100
maxShrinks = 1000


check :: Testable prop => prop -> IO ()
check prop = putStr "Testing" >> loop 1
  where
    loop i | i<maxTests = do
               t@(Node r _) <- sample $ unProp $ property prop
               if ok r then putChar '.' >> loop (i+1)
                 else do printf "\nFailed after %d tests:\n" i
                         printTestCase r
                         putStr "Shrinking"
                         r' <- shrinkLoop t
                         putStrLn "Minimal failure case:"
                         printTestCase r'
    loop i = printf "\nOK, passed %d tests.\n" i

-- | traverse result tree looking for a smaller failure example
shrinkLoop :: Tree Result -> IO Result
shrinkLoop t@(Node r ts) = loop maxShrinks t
  where
    loop :: Int -> Tree Result -> IO Result
    loop availShrinks t@(Node r ts)
      | availShrinks > 0 = 
          case dropWhile (ok.rootLabel.snd) (zip [0..] ts) of
            [] -> putChar '\n' >> return r
            ((k,t):_) -> putChar '.' >> loop (availShrinks-k) t
    loop _ t@(Node r _) = do
      printf "\nGave up after %d shrinking attempts!\n" maxShrinks
      return r
                                  
printTestCase = mapM_ putStrLn . testCase

-----------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------

-- pull out generator from a monadic computation
promote :: Monad m => m (Gen a) -> Gen (m a)
promote m = do
  eval <- delay
  return (liftM eval m)

delay :: Gen (Gen a -> a)
delay = MkGen (\s g -> runGen g s)


joinTree :: Tree (Tree a) -> Tree a
joinTree (Node (Node x xs) xss) = Node x (map joinTree xss ++ xs)
