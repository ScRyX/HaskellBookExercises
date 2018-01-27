-- Data.Char
import Data.Char

filterUpper :: String -> String
filterUpper s = filter isUpper s

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x) : xs

shout :: String -> String
shout "" = ""
shout (x:xs) = (toUpper x) : (shout xs)

capFirstLetter :: String -> Char
capFirstLetter "" = '_' 
capFirstLetter (x:xs) = toUpper x


-- Caesar cipher is in cipher.hs


-- Implementing your own standard functions
myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (_:xs) = myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x       = True
  | otherwise = myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) 
  | e == x    = True
  | otherwise = myElem e xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = rev l []
  where rev [] acc     = acc
        rev (x:xs) acc = rev xs (x:acc)


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish . map f $ xs 


squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = foldl (\acc x -> if comp acc x == LT then x else acc) x xs 


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = foldl (\acc x -> if comp acc x == GT then x else acc) x xs 


myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs



myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs

