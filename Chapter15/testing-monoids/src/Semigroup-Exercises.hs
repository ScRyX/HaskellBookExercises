{-# LANGUAGE DeriveGeneric #-}

module Exercises where

import Data.Semigroup
import Test.QuickCheck
import GHC.Generics

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  ((a <> b) <> c) == (a <> (b <> c))

--------
-- 1) --
--------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = 
  Trivial -> Trivial -> Trivial -> Bool
--------
-- 2) --
--------
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  x <> y = x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentAssoc a = (Identity a -> Identity a -> Identity a -> Bool)
--------
-- 3) --
--------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) 
       => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
--------
-- 6) --
--------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False 

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool
--------
-- 8) --
--------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd b <> _ = Snd b
  Fst a1 <> Fst a2 = Fst a2
  Fst a <> Snd b = Snd b

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    elements [Fst a, Snd b] 

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool
--------
-- 9) --
--------
-- reorder :: (a -> Gen b) -> Gen (a -> b)
-- reorder f = (\b c a -> f a b c)

newtype Combine a b =
  Combine { unCombine :: (a -> b) } 

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> (f x) <> (g x)
-- newtype Gen b = StdGen -> Size -> b
-- means that (a -> Gen) b ~~ a -> StdGen -> Size -> b
-- we have (a -> Gen b) ~~ a -> StdGen -> Size -> b
-- but want Gen (a -> b) ~~ StdGen -> Size -> a -> b

-- instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--  arbitrary = do
--    return (\x -> coarbitrary x arbitrary)
--------
-- 10)--
--------
-- same as 9) but <> will do function composition
--------
-- 11)--
--------
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Exercises.Failure a1) <> (Exercises.Failure a2) = Exercises.Failure (a1 <> a2)
  (Exercises.Failure a1) <> _            = Exercises.Failure a1
  _            <> (Exercises.Failure a)  = Exercises.Failure a
  (Exercises.Success b1) <> (Exercises.Success b2) = Exercises.Success b2

instance (Arbitrary a, Arbitrary b)
          => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Exercises.Failure a, Exercises.Success b]

type ValidationAssoc a b = (Validation a b) 
                        -> (Validation a b) 
                        -> (Validation a b) 
                        -> Bool 
--------
-- 12)--
--------
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  AccumulateRight (Exercises.Failure a1) <> AccumulateRight (Exercises.Failure a2) = AccumulateRight (Exercises.Failure a1)
  AccumulateRight (Exercises.Failure a1) <> _            = AccumulateRight (Exercises.Failure a1)
  _            <> AccumulateRight (Exercises.Failure a)  = AccumulateRight (Exercises.Failure a)
  AccumulateRight (Exercises.Success b1) <> AccumulateRight (Exercises.Success b2) = AccumulateRight (Exercises.Success (b1 <> b2))

instance (Arbitrary a, Arbitrary b)
          => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateRight $ (Exercises.Failure a), AccumulateRight (Exercises.Success b)]

type AccumulateRightAssoc a b = (AccumulateRight a b) 
                        -> (AccumulateRight a b) 
                        -> (AccumulateRight a b) 
                        -> Bool 
--------
-- 13)--
--------
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Exercises.Failure a1) <> AccumulateBoth (Exercises.Failure a2) = AccumulateBoth (Exercises.Failure (a1 <> a2))
  AccumulateBoth (Exercises.Failure a1) <> _                                     = AccumulateBoth (Exercises.Failure a1)
  _            <> AccumulateBoth (Exercises.Failure a)  = AccumulateBoth (Exercises.Failure a)
  AccumulateBoth (Exercises.Success b1) <> AccumulateBoth (Exercises.Success b2) = AccumulateBoth (Exercises.Success (b1 <> b2))

instance (Arbitrary a, Arbitrary b)
          => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateBoth $ (Exercises.Failure a), AccumulateBoth (Exercises.Success b)]

type AccumulateBothAssoc a b = (AccumulateBoth a b) 
                        -> (AccumulateBoth a b) 
                        -> (AccumulateBoth a b) 
                        -> Bool 

runTests :: IO ()
runTests = do
  putStrLn "Testing assoc for Trivial..."
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "Testing assoc for (Identity String)..."
  quickCheck (semigroupAssoc :: IdentAssoc String)
  putStrLn "Testing assoc for (Two (Sum Int) String)..."
  quickCheck (semigroupAssoc :: TwoAssoc (Sum Int) String)
  putStrLn "Testing assoc for BoolConj..."
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "Testing assoc for BoolConj..."
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "Testing assoc for Or Int String..."
  quickCheck (semigroupAssoc :: OrAssoc Int String)
  putStrLn "Testing assoc for Validation String Int ..."
  quickCheck (semigroupAssoc :: ValidationAssoc String Int)
  putStrLn "Testing assoc for AccumulateRight Int String..."
  quickCheck (semigroupAssoc :: AccumulateRightAssoc Int String)
  putStrLn "Testing assoc for AccumulateBoth Int String..."
  quickCheck (semigroupAssoc :: AccumulateBothAssoc (Sum Int) String)


