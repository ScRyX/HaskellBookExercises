module Addition where

import Test.QuickCheck
import Test.Hspec

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3] 

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a,b) -- could also be e.g. Gen (Either a b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)
-- used like this: sample' (genTuple :: Gen (Int, Float))

genThreeple :: (Arbitrary a, Arbitrary b,
                Arbitrary c)
            => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
-----------------------------------------
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always\
       \ greater than x" $ do
       property $ \x -> x + 1 > (x :: Int)
