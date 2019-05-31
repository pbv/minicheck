{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Test.Minicheck where

import qualified System.Random as R
import           Data.Tree       --  multiway trees a.k.a. rose trees 
import           Control.Monad
import           Text.Printf

--
-- | Generators
--
newtype Gen a = MkGen { runGen :: R.StdGen -> a }
              deriving (Functor)

-- NB: this is not the reader monad (StdGen ->)
-- because we need to split the random generator 
instance Monad Gen where
  return x = MkGen (\_ -> x)
  m >>= f = MkGen (\s -> let (s', s'') = R.split s
                         in runGen (f (runGen m s')) s'')

-- Applicative instance implied by the monadic operations;
-- evaluation order should not matter for "well-behaved" RNGs split
instance Applicative Gen where
  pure = return
  mf <*> ma = do  f<-mf; a<-ma; return (f a)


-- | sample a generator
sample :: Gen a -> IO a
sample gen = do
  (s,s') <- R.split <$> R.getStdGen
  R.setStdGen s'
  return (runGen gen s)

--
-- type class for generation and shrinking 
-- 
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink = const []  -- default: no shrinking

--
-- properties and testing
--
data Result
  = MkResult { testPass :: Bool -- ^ True if test passed, False otherwise
             , testCase :: [String] -- ^ for reporting failed tests
             }
  deriving Show

-- | A property is a generator for (lazy) trees of results
-- obtained by unfolding the shrinking function
--
newtype Prop = MkProp { unProp :: Gen (Tree Result) }

liftBool :: Bool -> Result
liftBool b = MkResult {testPass=b, testCase=[]}

mapResult :: Testable prop => (Result -> Result) -> prop -> Prop
mapResult f = MkProp . fmap (fmap f). unProp . property 

-- | Adds the given string to the counterexample
counterexample :: Testable prop => String -> prop -> Prop
counterexample s = mapResult (\r -> r{ testCase = s:testCase r }) 

-- | class for things that can be tested,
-- i.e. made into into a property
--
class Testable a where
  property :: a -> Prop

instance Testable Prop where
  property p = p

instance Testable Bool where
  property b =
    MkProp $ return $ Node { rootLabel = liftBool b, subForest=[] }

-- | for testing functions, we need an arbitrary instance
-- for the argument
instance (Arbitrary a, Show a, Testable b)
         => Testable (a -> b) where
  property pf = forAllShrink arbitrary shrink pf

-- | quantification given a generator and a shrinking function
forAllShrink :: (Show a, Testable prop)
             =>  Gen a -> (a -> [a]) -> (a -> prop) -> Prop
forAllShrink gen shrinker pf = MkProp $ do
  a <- gen
  unProp $ shrinking shrinker a pf

-- | quantification with no shrinking
forAll :: (Show a, Testable prop)
       => Gen a -> (a -> prop) -> Prop
forAll gen = forAllShrink gen (const []) 

-- | generate the shrinking tree for an argument to a property
-- first use `props' to build a       Tree (Gen (Tree Result))
-- the Tree monad instance and `promote' to  get
---                                  Gen (Tree (Tree Result)
-- finally use `joinTree` to get     Gen (Tree Result) 
shrinking :: (Testable prop, Show a)
          => (a -> [a]) -> a -> (a -> prop) -> Prop
shrinking shrinker x0 pf 
  = MkProp $ fmap joinTree $ promote $ props x0
 where
   props x = Node { rootLabel = unProp $
                                counterexample (show x) $
                                property $ pf x
                  , subForest = [ props x' | x' <- shrinker x ]
                  }

-- helper functions taken from the QuickCheck source
-- pull out generator from a monadic computation
promote :: Monad m => m (Gen a) -> Gen (m a)
promote m = do
  eval <- delay
  return (liftM eval m)

-- generate running function for the Gen monad;
-- this captures the current random generator and so is "unsafe"
delay :: Gen (Gen a -> a)
delay = MkGen (\s g -> runGen g s)

-- flatten a tree
joinTree :: Tree (Tree a) -> Tree a
joinTree (Node (Node x xs) xss) = Node x (map joinTree xss ++ xs)
 -- alternative: Node x (xs ++ map joinTree xss)
 -- corresponds to the join operation of the Tree monad
 -- but this choice leads to better shrinking




-- | check a property and shrink to a minimal counterexample
--
check :: Testable prop => prop -> IO ()
check prop = checkWith (minimize greedySearch) prop


-- | print the test case in a result
printTestCase :: Result -> IO ()
printTestCase = mapM_ putStrLn . testCase

-- | check a property and
-- print the shrinking tree for the counter-example 
checkTree :: Testable prop => prop -> IO ()
checkTree   = checkWith (putStr . drawTree . fmap show) 

-- | check a property with provided callback for failure 
checkWith :: Testable prop => (Tree Result -> IO ()) -> prop -> IO ()
checkWith callback prop  = putStr "Testing" >> loop 1
  where
    loop i | i<maxTests = do
               t@(Node r _) <- sample $ unProp $ property prop
               if testPass r then putChar '.' >> loop (i+1)
                 else do printf "\nFalsified after %d tests!\n" i
                         callback t
    loop i = printf "\nOK, passed %d tests.\n" i


-- | callback that applies a search strategy
-- for minimizing the counterexample
minimize :: (Tree Result -> (Int, Result)) -> Tree Result -> IO ()
minimize search t@(Node r _) = do
  putStrLn "Failed test case:"
  printTestCase r
  let (n, r') = search t
  printf "Shrinking... %d steps\n" n
  putStrLn "Minimal test case:"
  printTestCase r'

         
-- | Simple greedy "hill-climbing" search strategy;
-- allways selects the first child that failsifies the test 
-- terminates when it reaches a local optimium
-- or when after `maxShrinks' steps (defined below)
greedySearch :: Tree Result -> (Int, Result)
greedySearch t = search 0 t
  where
    search i (Node r ts)
      | i < maxShrinks
      = case dropWhile (testPass.rootLabel) ts of
          [] -> (i, r)
          (t':_) -> search (i+1) t'
      | otherwise = (i, r)


--
-- Testing parameters
--
maxTests, maxShrinks :: Int
maxTests = 1000
maxShrinks = 1000


--
-- debugging
--
-- | draw the shrinking tree for a property
propTree :: Testable prop => prop -> IO ()
propTree prop = do
  tree <- sample (unProp $ property prop) 
  putStr $ drawTree (fmap show tree)

-- | prune a tree to a max number of levels
prune :: Int -> Tree a -> Tree a
prune n (Node x ts)
  | n > 0 = Node x (map (prune (n-1)) ts)
  | otherwise = Node x []
