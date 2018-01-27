{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

funcId :: (Functor f, Eq (f a)) =>
               f a
            -> Bool
funcId f = fmap id f == f

funcComp :: (Eq (f c), Functor f) =>
              f a
           -> Fun a b
           -> Fun b c
           -> Bool 
funcComp x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

--------
-- 1) --
--------
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ Finance
             , Desk a
             , Bloor b
             ]

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

type QuantIntIntFC =
  Quant Int Int -> IntToInt -> IntToInt -> Bool
--------
-- 2) --
--------
data K a b =
  K a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do 
    a <- arbitrary
    return (K a)

instance Functor (K a) where
  fmap _ (K a) = K a

type KIntIntFC =
  K Int Int -> IntToInt -> IntToInt -> Bool
--------
-- 3) --
--------
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return (Flip (K a))

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)
  -- nasledovne nefunguje, wtf?
  --fmap f (Flip (K a)) = Flip $ fmap f (K a) 

type FlipKIntIntFC = 
  Flip K Int Int -> IntToInt -> IntToInt -> Bool
--------
-- 4) --
--------
data EvilConst a b =
  EvilConst b
  deriving (Eq, Show)

instance (Arbitrary b) => Arbitrary (EvilConst a b) where
  arbitrary = do
    b <- arbitrary
    return (EvilConst b)
 
instance Functor (EvilConst a) where
  fmap f (EvilConst b) = EvilConst $ f b

type EvilConstIntIntFC = 
  EvilConst Int Int -> IntToInt -> IntToInt -> Bool
--------
-- 5) --
--------
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (LiftItOut [] a) where
  arbitrary = do
    a <- arbitrary
    return (LiftItOut [a])

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut (fa)) = LiftItOut $ fmap f fa

type LiftItOutFC = 
  LiftItOut [] Int -> IntToInt -> IntToInt -> Bool 
--------
-- 6) --
--------
data Parappa f g a =
  DaWrappa (f a) (g a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Parappa [] Maybe a) where
  arbitrary = do
    a <- arbitrary
    return $ DaWrappa ([a]) (Just a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa (fa) (ga)) = DaWrappa (fmap f fa) (fmap f ga) 

type ParappaFC = 
  Parappa [] Maybe Int -> IntToInt -> IntToInt -> Bool
--------
-- 7) --
--------
data IgnoreOne f g a b =
  IgnoringSth (f a) (g b)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (IgnoreOne [] Maybe a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (IgnoringSth [a] $ Just b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSth fa gb) = IgnoringSth fa $ fmap f gb

type IgnoreOneFC = 
  IgnoreOne [] Maybe Int Int -> IntToInt -> IntToInt -> Bool
--------
-- 9) --
--------
data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, Cons a Nil, Cons a (Cons a Nil)]

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a rem) = Cons (f a) (fmap f rem)

type ListFC =
  List Int -> IntToInt -> IntToInt -> Bool
--------
--10) --
--------
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq,Show)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    elements [ NoGoat
             , OneGoat a1
             , MoreGoats (OneGoat a1) NoGoat NoGoat
             , MoreGoats NoGoat (OneGoat a2) NoGoat 
             , MoreGoats NoGoat NoGoat (OneGoat a2) 
             ]
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

type GoatLordFC = 
  GoatLord Int -> IntToInt -> IntToInt -> Bool
--------
--11) --
--------
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)
  deriving (Show, Eq)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read sToa) = Read (f . sToa)


  
runTests :: IO ()
runTests = do
  putStrLn "Testing Quant Functor..."
  quickCheck (funcId :: (Quant Int Int) -> Bool)
  quickCheck (funcComp :: QuantIntIntFC)
  putStrLn "Testing K Functor..."
  quickCheck (funcId :: (K Int Int) -> Bool)
  quickCheck (funcComp :: KIntIntFC)
  putStrLn "Testing Flip Functor..."
  quickCheck (funcId :: (K Int Int) -> Bool)
  quickCheck (funcComp :: FlipKIntIntFC)
  putStrLn "Testing EvilConst Functor..."
  quickCheck (funcId :: (EvilConst Int Int) -> Bool)
  quickCheck (funcComp :: EvilConstIntIntFC)
  putStrLn "Testing LiftItOut Functor..."
  quickCheck (funcId :: (LiftItOut [] Int) -> Bool)
  quickCheck (funcComp :: LiftItOutFC)
  putStrLn "Testing Parappa Functor..."
  quickCheck (funcId :: (Parappa [] Maybe Int) -> Bool)
  quickCheck (funcComp :: ParappaFC)
  putStrLn "Testing IgnoreOne Functor..."
  quickCheck (funcId :: (IgnoreOne [] Maybe Int Int) -> Bool)
  quickCheck (funcComp :: IgnoreOneFC)
  putStrLn "Testing List Functor..."
  quickCheck (funcId :: (List Int) -> Bool)
  quickCheck (funcComp :: ListFC)
  putStrLn "Testing GoatLord Functor..."
  quickCheck (funcId :: (GoatLord Int) -> Bool)
  quickCheck (funcComp :: GoatLordFC)

