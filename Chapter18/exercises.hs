module ChapterExercises where

import Control.Monad

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma =  ma >>= (return . f)

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = (ma >>= (return . f)) <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f) 


flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
