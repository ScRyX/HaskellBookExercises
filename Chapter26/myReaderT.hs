module MyReaderT where

import Control.Applicative

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) 
      => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT (\r -> f <$> rma r)

instance (Applicative m)
      => Applicative (ReaderT r m) where
  pure x = ReaderT (\r -> pure x) 
  (ReaderT mfab) <*> (ReaderT ma) = ReaderT (\r -> (mfab r) <*> (ma r))

instance (Monad m)
      => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ (\r -> (rma r) >>= (\a -> ((runReaderT . f) a) r))

config :: [Int]
config = [1,2,3]

