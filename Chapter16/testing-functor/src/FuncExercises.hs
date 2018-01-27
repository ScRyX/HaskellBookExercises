module FuncExercises where

import Test.QuickCheck
import Test.QuickCheck.Function

import Tests (funcId, funcComp')

--------
-- 1) --
--------
newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type StringToString = Fun String String 
type IdentityStringFC =
  Identity String -> StringToString -> StringToString -> Bool
--------
-- 2) --
--------
data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

type PairStringFC =
  Pair String -> StringToString -> StringToString -> Bool
--------
-- 3) --
--------
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Two a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

type TwoStringStringFC =
  Two String String -> StringToString -> StringToString -> Bool
--------
-- 5) --
--------
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Three' a b) where
  arbitrary = do 
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Three' a b1 b2)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

type ThreeStringStringFC =
  Three' String String -> StringToString -> StringToString -> Bool
--------
-- 7) --
--------
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Four' a b) where
  arbitrary = do 
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 $ f b

type FourStringStringFC =
  Four' String String -> StringToString -> StringToString -> Bool
--------
-- 8) --
--------
-- Nope, Trivial is of kind '*'
--------------
-- Possibly --
--------------
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq,Show)

instance Functor Possibly where 
  fmap f (Yeppers a) = Yeppers $ f a 
  fmap f LolNope = LolNope

instance (Arbitrary a) 
       => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    elements [LolNope, Yeppers a]

type PossiblyStringFC =
  Possibly String -> StringToString -> StringToString -> Bool
--------------
-- Sum a b  --
--------------
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) 
       => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

type TwoStringStringFC' =
  Two String String -> StringToString -> StringToString -> Bool

runTests :: IO ()
runTests = do
  putStrLn "Testing Identity Functor"
  quickCheck (funcId :: (Identity Int) -> Bool)
  quickCheck (funcComp' :: IdentityStringFC)
  putStrLn "Testing Pair Functor"
  quickCheck (funcId :: (Pair Int) -> Bool)
  quickCheck (funcComp' :: PairStringFC)
  putStrLn "Testing Two Functor"
  quickCheck (funcId :: (Two String String) -> Bool)
  quickCheck (funcComp' :: TwoStringStringFC)
  putStrLn "Testing Three' Functor"
  quickCheck (funcId :: (Three' String String) -> Bool)
  quickCheck (funcComp' :: ThreeStringStringFC)
  putStrLn "Testing Four' Functor"
  quickCheck (funcId :: (Four' String String) -> Bool)
  quickCheck (funcComp' :: FourStringStringFC)
  putStrLn "Testing Possibly Functor"
  quickCheck (funcId :: (Possibly String) -> Bool)
  quickCheck (funcComp' :: PossiblyStringFC)
  putStrLn "Testing Two (Either) Functor"
  quickCheck (funcId :: (Two String String) -> Bool)
  quickCheck (funcComp' :: TwoStringStringFC')

