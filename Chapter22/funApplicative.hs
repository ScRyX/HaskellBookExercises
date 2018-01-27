{-# LANGUAGE InstanceSigs #-}

module FunctionApplicative where

import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a } 

newtype HumanName =
  HumanName String deriving (Eq, Show)

newtype DogName =
  DogName String deriving (Eq, Show)

newtype Address =
  Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra 

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (\_ -> x)

  (<*>) :: Reader r (a -> b) 
      -> Reader r a 
      -> Reader r b 
  Reader rab <*> Reader ra = Reader $ (\r -> rab r (ra r))

