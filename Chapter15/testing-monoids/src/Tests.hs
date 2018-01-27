module Tests where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

asc :: Eq a
    => (a -> a -> a)
    -> a -> a -> a
    -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c


monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)


monoidLeftId :: (Eq m, Monoid m)
             => m -> Bool
monoidLeftId x = (mempty <> x) == x
monoidRightId :: (Eq m, Monoid m)
             => m -> Bool
monoidRightId x = (x <> mempty) == x

type S = String
type B = Bool

-------------------------------------------

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = 
  Bull -> Bull -> Bull -> Bool

-------------------------------------------
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend x (First' Nada) = x 
  mappend (First' Nada) x = x 
  mappend (First' (Only x)) (First' (Only y)) = First' (Only x)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return (First' (Only a)))
              , (1, return (First' Nada))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = 
     First' String
  -> First' String
  -> First' String
  -> Bool
type FstId = First' String -> Bool


runTests :: IO ()
runTests = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftId :: FstId)
  quickCheck (monoidRightId :: FstId)
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftId :: Bull -> Bool)
  quickCheck (monoidRightId :: Bull -> Bool)
  quickCheck (monoidAssoc :: S -> S -> S -> B)
  quickCheck (monoidLeftId :: S -> B)
  quickCheck (monoidRightId :: S -> B)
