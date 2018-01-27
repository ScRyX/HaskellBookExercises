module CustomEitherMonad where

import Control.Applicative
import Control.Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  First e <*> _ = First e 
  Second f <*> r = fmap f r

instance Monad (Sum a) where
  return = pure
  First e >>= _ = First e
  Second a >>= f = Second $ f a

