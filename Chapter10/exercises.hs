-- 1
-- A
stops = "pbtdkg"
vowels = "aeiou"
stopVowelStop s v = [(s1,v1,s2) | s1 <- s, v1 <- v, s2 <- s]
stopVowelStop2 s v = [('p',v1,s2) | v1 <- v, s2 <- s]


-- Using folds
myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = foldr (\a b -> a == x || b) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (==x)

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a):b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = foldl (\acc a -> if comp a acc == GT then a else acc) x xs
-- Foldr nejde na myMaxBy (\_ _ -> GT) [1..10] lebo neprejde cely list?
--myMaximumBy comp (x:xs) = foldr (\a acc -> if comp a acc == GT then a else acc) x xs

