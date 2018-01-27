module Exercises where

import Data.Monoid
import Test.QuickCheck

mAssoc :: (Eq m, Monoid m)
       => m -> m -> m -> Bool
mAssoc a b c =
  ((a <> b) <> c) == (a <> (b <> c))

mLeftId :: (Eq m, Monoid m)
        => m -> Bool  
mLeftId x = mempty <> x == x

mRightId :: (Eq m, Monoid m)
        => m -> Bool  
mRightId x = x <> mempty == x

--------
-- 2) --
--------
newtype Identity a =
  Identity a deriving (Eq, Show)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  (Identity a1) `mappend` (Identity a2) = Identity (a1 `mappend` a2)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
type IdentAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
--------
-- 3) --
--------
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  (Two x1 y1) `mappend` (Two x2 y2) = Two (x1 `mappend` x2) (y1 `mappend` y2)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
--------
-- 4) --
--------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True 
  BoolConj True `mappend` BoolConj True = BoolConj True
  _ `mappend` _ = BoolConj False 

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool
--------
-- 6) --
--------
newtype Combine a b =
  Combine { unCombine :: (a -> b) } 

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  Combine f `mappend` Combine g = Combine $ \x -> (f x) `mappend` (g x)
--------
-- 8) --
--------
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  Mem f `mappend` Mem g = Mem $ (\x -> let (a,b) = f x
                                           (c,d) = g b
                                           in (a `mappend` c,d))

f' = Mem $ \s -> ("hi", s + 1)


runTests :: IO ()
runTests = do
  putStrLn "Testing Identity String..."
  putStr "> Assoc:\t"
  quickCheck (mAssoc :: IdentAssoc (Sum Int))
  putStr "> Right Identity:\t"
  quickCheck (mRightId :: (Identity String -> Bool))
  putStr "> Left Identity:\t"
  quickCheck (mLeftId :: (Identity String -> Bool))
  putStrLn ""

  putStrLn "Testing Two String String..."
  putStr "> assoc:\t"
  quickCheck (mAssoc :: TwoAssoc String String)
  putStr "> right identity:\t"
  quickCheck (mRightId :: (Two String String -> Bool))
  putStr "> left identity:\t"
  quickCheck (mLeftId :: (Two String String -> Bool))
  putStrLn ""

  putStrLn "Testing BoolConj..."
  putStr "> assoc:\t"
  quickCheck (mAssoc :: BoolConjAssoc)
  putStr "> right identity:\t"
  quickCheck (mRightId :: (BoolConj -> Bool))
  putStr "> left identity:\t"
  quickCheck (mLeftId :: (BoolConj -> Bool))
  putStrLn ""

  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

