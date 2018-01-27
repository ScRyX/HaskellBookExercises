module MyMaybeT where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.IO.Class

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) } 

instance (Functor m)
      => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
     => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ liftA2 (<*>) fab mma 

instance (Monad m)
      => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma -- v :: Maybe a
    case v of
      Nothing -> return Nothing
      Just x -> runMaybeT $ f x

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (MonadIO m)
      => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

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

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither ma
  where swapEither (Left e) = Right e
        swapEither (Right a) = Left a

eitherT :: Monad m => 
          (a -> m c)
       -> (b -> m c)
       -> EitherT a m b
       -> m c
eitherT fac fbc (EitherT ma) = ma >>= ei where
  ei (Left a)  = fac a
  ei (Right b) = fbc b


----------------------
-- PLAYING WITH IO Maybe
--
justIfEven :: Int -> Maybe Int
justIfEven x
  | even x = Just x
  | otherwise = Nothing

addT :: Int -> Int -> IO (Maybe Int)
addT n1 n2 = runMaybeT $ do
  a <- MaybeT $ (return :: a -> IO a) $ justIfEven n1 
  b <- MaybeT $ (return :: a -> IO a) $ justIfEven n2 
  return (a + b)
