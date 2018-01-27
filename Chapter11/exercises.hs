import Data.Char
import qualified Data.Map as Map
-- multiple choice: 
-- 1A, 2C, 3C, 4C


-- Ciphers
toIndex :: Char -> Int
toIndex c = ord c - ord 'a'

fromIndex :: Int -> Char
fromIndex i = chr $ ord 'a' + i

shiftChar :: Int -> Char -> Char
shiftChar s c = fromIndex . (`mod` 26) . (+s) . toIndex $ c

cipher :: Int -> String -> String
cipher key s = map (shiftChar key) s

decipher :: Int -> String -> String
decipher key s = map (shiftChar $ negate key) s

newtype VigenereKey = VigenereKey String

repeatToLength :: String -> Int -> String
repeatToLength s n = take n $ sRepeat
  where sRepeat = s ++ sRepeat

shiftAmounts :: String -> [Int]
shiftAmounts "" = []
shiftAmounts (x:xs) = (:shiftAmounts xs) $ toIndex x

vigenere :: VigenereKey -> String -> String
vigenere _ "" = ""
vigenere (VigenereKey k) text = 
  [ciph c amt | (c,amt) <- zip text shifts] 
  where
    shifts = shiftAmounts . repeatToLength k $ (length text) 
    ciph c s = shiftChar s c


-- As-patterns
-- 1)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf pattern@(x:xs) (t:ts)
  | t == x = isSubseqOf xs ts
  | otherwise = isSubseqOf pattern ts

-- 2)
capitalizeWords :: String -> [(String,String)]
capitalizeWords w = map f $ words w
  where f word@(x:xs) = (word, (:xs) $ toUpper x)


-- Language exercises
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (:xs) $ toUpper x


capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (x:xs)
  | isLetter x = ((:takeWhile notPeriod xs) $ toUpper x) 
                 ++ (capitalizeParagraph $ dropWhile notPeriod xs)
  | otherwise = (:capitalizeParagraph xs) x
  where notPeriod = (/='.')


-- Phone exercise
convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

newtype Symbol = Symbol Char
data Button = Button Symbol [Char]
type DaPhone = [Button]

phone = 
  [ Button (Symbol '1') "1", Button (Symbol '2') "abc2", Button (Symbol '3') "def3",
    Button (Symbol '4') "ghi4", Button (Symbol '5') "jkl5", Button (Symbol '6') "mno6",
    Button (Symbol '7') "pqrs7", Button (Symbol '8') "tuv8", Button (Symbol '9') "wxyz9",
    Button (Symbol '*') "^", Button (Symbol '0') " 0", Button (Symbol '#') ".,"
  ]

processBtn :: Button -> [(Char,[(Char,Int)])]
processBtn (Button (Symbol s) values) = go [] $ zip values [1..]
  where 
    go acc [] = acc
    go acc ((c,n):xs)
      | isLetter c = go ((c,[(s,n)]):(toUpper c,[('*',1),(s,n)]):acc) xs
      | otherwise  = go ((c,[(s,n)]):acc) xs

keyMapping :: DaPhone -> [[(Char,[(Char,Int)])]]
keyMapping phone = map f phone
  where f btn = processBtn btn

charToTapsMap :: DaPhone -> Map.Map Char [(Char,Int)]
charToTapsMap phone = Map.fromList $ concat . keyMapping $ phone

stringToTaps :: String -> [(Char,Int)]
stringToTaps s = concat . map (\c -> findTaps c) $ s
  where 
  m = charToTapsMap phone
  findTaps c = 
    case Map.lookup c $ m of
    Just x -> x
    Nothing -> undefined

convoToTaps :: [[(Char,Int)]]
convoToTaps = map stringToTaps convo


fingerTaps :: [(Char,Int)] -> Int
fingerTaps = sum . map (\(_,i) -> i)

-- Hutton's razor
data Expr
   = Lit Integer
   | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
