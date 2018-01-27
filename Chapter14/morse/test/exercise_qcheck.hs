module QuickCheck_Exercises where

import Test.QuickCheck
import Data.List (sort, reverse)
import Test.QuickCheck.Function (apply, Fun(..))

half x = x / 2

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = (x==) . (*2) . half $ x

----------------------------------------

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_intSort :: Property 
prop_intSort =
  forAll (vector 10 :: (Gen [Int])) 
  ((\arr -> listOrdered . sort $ arr))

----------------------------------------
-- 3)
plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x
prop_plusAssoc :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssoc x y z = x + (y + z) == (x + y) + z
prop_plusComm :: (Eq a, Num a) => a -> a -> Bool
prop_plusComm x y = x + y == y + x
-- 5)
prop_quot_rem :: (Integral a) => a -> NonZero a -> Bool
prop_quot_rem x (NonZero y) = (quot x y)*y + (rem x y) == x
prop_div_mod :: (Integral a) => a -> NonZero a -> Bool
prop_div_mod x (NonZero y) = (div x y)*y + (mod x y) == x
-- 6)
prop_pow_comm :: (Num a, Integral a) => a -> a -> Bool
prop_pow_comm x y = x ^ y == y ^ x
-- 7)
prop_list_rev :: (Eq a ) => [a] -> Bool
prop_list_rev xs = (reverse . reverse $ xs) == id xs

----------------------------------------
-- 8)
applyProp :: Fun Int Int -> Int -> Bool
applyProp (Fun _ f) a = (f $ a) == (f a)

-- 9) FALSE!
prop_foldr_concat :: [Int] -> [Int] -> Bool
prop_foldr_concat xs ys = foldr (:) xs ys == xs ++ ys

-- TRUE!
prop_foldr_concat2 :: [[Int]] -> Bool
prop_foldr_concat2 xs = foldr (++) [] xs == concat xs


-- 10) NOPE! (false for n = 2, xs = [])
f n xs = length (take n xs) 
prop_f_length_and_take :: Int -> [Int] -> Bool
prop_f_length_and_take n xs = f n xs == n

-- 11)
f' x = (read (show x))
--prop_readNshow :: (Read a, Show a, Eq a) => a -> Bool
prop_readNshow :: String -> Bool
prop_readNshow x = f' x == x

----------------------------------------
-- IDEMPOTENCE
twice f = f . f
fourTimes = twice . twice


prop_idem_1 :: String -> Bool
prop_idem_1 x = (sort x
                == twice sort x)
                &&
                (sort x
                == fourTimes sort x)



----------------------------------------
-- Custom Gens
data Fool =
    Fulse
  | Frue
  deriving (Eq,Show)

foolGen :: Gen Fool
foolGen = do
  oneof [return Fulse, return Frue]

foolGenBiased :: Gen Fool
foolGenBiased = do
  frequency [(2,return Fulse), (1, return Frue)]
