{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Ratio ((%))

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)  

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >> 
      (Left <$> integer)
  <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

type DecimalOrFraction = 
  Either Integer Rational

decOrFrac :: String
decOrFrac = [r|
1/2
1
3/5
|]

parseDecRat :: Parser DecimalOrFraction
parseDecRat =
  skipMany (oneOf "\n")
  >>
      (Right <$> try virtuousFraction)
  <|> (Left <$> decimal)

main = do
  let p f i =
        parseString f mempty i
  --print $ p (some (token parseNos)) eitherOr
  print $ p (some (token parseDecRat)) decOrFrac 

