module WordNumber where
import Data.List
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
