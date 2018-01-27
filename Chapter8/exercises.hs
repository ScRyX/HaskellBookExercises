import Data.List (intersperse)
--------------------------------
-- 1D, 2B, 3D, 4B


-- Currying review
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"


-- Recursion
-- 2) Sum first n natural numbers
sumOfFirst :: (Eq a, Num a) => a -> a
sumOfFirst 0 = 0
sumOfFirst 1 = 1
sumOfFirst n = n + (sumOfFirst $ n - 1)

-- 3) Multiplication via repeated summation
recMult :: (Integral a) => a -> a -> a
recMult a b
  | a < b = go a b 0
  | otherwise = go b a 0
  where go 0 _ acc   = acc 
        go x y acc = go (x - 1) y $ acc + y 




-- Fix dividedBy
dividedBy :: Integral a => a -> a -> Maybe a
dividedBy _ 0 = Nothing
dividedBy num denom = sign $ go (abs num) (abs denom) 0
  where
    sign = fmap $ (*) $ signum num * signum denom
    go n d count
      | n < d = Just count 
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 Function
mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 $ mc91 $ x + 11

-- Numbers into words


digitToWord :: Int -> String
digitToWord n = 
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "unknown"

digits :: Int -> [Int]
digits n = go n 1 []
  where 
  go n d acc
    | n `mod` (d * 10) == n = (:acc) $ getDigit n d
    | otherwise = go n (d * 10) $ (:acc) $ getDigit n d
    where getDigit n d = (n `mod` (d * 10)) `div` d

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits 

