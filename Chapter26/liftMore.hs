module LiftMore where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans

-- EitherT
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }


instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m
      => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT ma) = EitherT $ liftA2 (<*>) fab ma

instance Monad m
      => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right 

-- StateT
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

instance MonadTrans (StateT s) where
  lift ma = StateT $ (\s -> liftM (\a -> (a,s)) ma) 
