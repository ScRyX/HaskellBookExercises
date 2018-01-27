module Main where

import Text.Trifecta
--import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

p1 :: CharParsing m => m String
p1 = string "1"
p12 :: CharParsing m => m String
p12 = string "12"
p123 :: CharParsing m => m String
p123 = string "123"

--parseMe = char '1' >> char '2' >> char '3'

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
