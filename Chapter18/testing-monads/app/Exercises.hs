module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad

----------
--- 1) ---
----------
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a
      => Arbitrary (Nope a) where
  arbitrary = 
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

----------
--- 2) ---
----------
data PhbtEither b a = Lft a | Rght b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Lft a) = Lft $ f a 
  fmap _ (Rght b) = Rght b


instance Applicative (PhbtEither b) where
  pure = Lft  
  Rght f <*> _ = Rght f 
  Lft _ <*> Rght b = Rght b
  Lft f <*> Lft b = Lft $ f b

instance Monad (PhbtEither b) where
  return = pure
  (Lft a) >>= f = f a 
  (Rght b) >>= _ = Rght b 

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Lft a, return $ Rght b]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

----------
--- 3) ---
----------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a 

instance Applicative Identity where
  pure = Identity 
  Identity f <*> ida = fmap f ida

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a 

instance (Arbitrary a)
      => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

----------
--- 4) ---
----------
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil 
  Nil <*> _ = Nil
  Cons f fs <*> Nil = Nil
  Cons f fs <*> xs = fmap f xs `join` (fs <*> xs)
                     where join Nil xs = xs
                           join (Cons x xs) ys = Cons x (xs `join` ys)

instance Monad List where
  return = pure
  Nil >>= f = Nil
  (Cons x xs) >>= f = f x `join` (xs >>= f) 
                     where join Nil xs = xs
                           join (Cons x xs) ys = Cons x (xs `join` ys)

instance (Arbitrary a)
      => Arbitrary (List a) where
  arbitrary = do
    as <- vector 10 
    return (foldr Cons Nil as) 

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

runTests :: IO()
runTests = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  
