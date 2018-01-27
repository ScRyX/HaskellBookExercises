{-# LANGUAGE ViewPatterns #-}
module Tests where

import Test.QuickCheck
import Test.QuickCheck.Function


funcId :: (Functor f, Eq (f a)) =>
                f a
             -> Bool
funcId f = fmap id f == f

funcComp :: (Eq (f c), Functor f) =>
              (a -> b)
             -> (b -> c)
             -> (f a)
             -> Bool 
funcComp f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

funcComp' :: (Eq (f c), Functor f) =>
                f a 
             -> Fun a b 
             -> Fun b c 
             -> Bool 
funcComp' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC =
  [Int] -> IntToInt -> IntToInt -> Bool

f :: [Int] -> Bool
f x = funcId x

main :: IO ()
main = do
  quickCheck f
  quickCheck (funcComp' :: IntFC)
