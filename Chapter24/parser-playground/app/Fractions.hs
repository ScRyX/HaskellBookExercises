{-# LANGUAGE OverloadedStrings #-}

module ParsingFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)  

parseReturnNumber :: Parser Integer
parseReturnNumber = do
  x <- integer
  eof
  return x
