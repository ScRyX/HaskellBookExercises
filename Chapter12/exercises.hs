import Data.List (intersperse)

-- Determine kinds
-- 1) a :: *
-- 2) a :: *, f :: * -> *


-- String processing
-- 1)
notThe :: String -> Maybe String
notThe w = if w == "the" then Nothing else Just w  

replaceThe :: String -> String
replaceThe s = concat . intersperse " " $ go [] $ words s
  where go acc [] = acc
        go acc (w:ws) = case notThe w of
          Nothing -> go (acc ++ ["a"]) ws
          Just sth -> go (acc ++ [sth]) ws

-- 2)
isVowel :: Char -> Bool
isVowel c = elem c "aeiou" 

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go 0 $ words s 
  where go acc []         = acc
        go acc [_]        = acc
        go acc (w1:w2:ws) 
          | w1 == "the" && (isVowel $ head w2)
            = go (acc + 1) (w2:ws)
          | otherwise = go acc (w2:ws)

-- 3)
countVowels :: String -> Int
countVowels = length . filter isVowel


-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s * 2 > length s = Nothing
  | otherwise = Just (Word' s)


-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (1+) $ natToInteger n


integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | i == 0    = Just Zero
  | otherwise = Just $ Succ $ convert (i - 1)
  where convert 0 = Zero
        convert i = Succ $ convert (i - 1)

