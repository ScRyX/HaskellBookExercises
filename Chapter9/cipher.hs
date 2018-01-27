module Cipher where

import Data.Char
import System.IO

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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Write a word to encrypt:"
  line <- getLine
  putStrLn  $ "The encrypted form is: "
           ++ (cipher 2 line)
