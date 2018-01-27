module FunctionMonad where

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a } 


foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t,Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a],Int)
froot = undefined 

