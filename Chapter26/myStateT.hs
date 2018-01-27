module MyStateT where

import Control.Applicative

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m)
      => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (\s -> fmap ((\x -> (x,s)) . f . fst) (smas s))

instance (Monad m)
      => Applicative (StateT s m) where
  pure x = StateT $ (\s -> return (x,s))
  (StateT fab) <*> (StateT ma) = 
    StateT $ (\s -> do
      (f,tempS) <- fab s
      (a,s1) <- ma tempS
      return (f a, s1)
    )

instance (Monad m)
      => Monad (StateT s m) where
  return = pure
  (StateT ma) >>= f = 
    StateT $ (\s -> do
      (a,tempS) <- ma s
      (runStateT $ f a) tempS
    )
    
