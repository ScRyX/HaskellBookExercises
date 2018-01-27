{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Exercises where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet

--------------------
-- SemVer Parsing --
--------------------
data NumberOrString = 
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = 
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer maj1 min1 p1 _ _) (SemVer maj2 min2 p2 _ _) = 
    compare (maj1, min1, p1) (maj2, min2, p2)

allowedChars :: CharSet
allowedChars = CharSet.build (\c -> elem c (('-':['a'..'z']) ++ ['A'..'Z']))

skipSepAndEof :: [Char] -> Parser ()
skipSepAndEof seps = skipMany (oneOf seps) <|> skipMany eof

parseNos :: Parser NumberOrString
parseNos = do
  (NOSS <$> some (oneOfSet allowedChars)) <|> (NOSI <$> decimal)

parseNosDot :: Parser NumberOrString
parseNosDot = do
  nos <- parseNos
  skipMany (oneOf ".")
  return nos

parseRelease :: Parser [NumberOrString]
parseRelease = do
  ((char '-' >> many parseNosDot) <|> return [])

parseMetadata :: Parser [NumberOrString]
parseMetadata = do
  ((char '+' >> many parseNosDot) <|> return [])

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal 
  _ <- char '.'
  minor <- decimal 
  _ <- char '.'
  patch <- decimal 
  release <- try parseRelease 
  metadata <- try parseMetadata 

  return (SemVer major minor patch release metadata)

-----------------------------
-- Postive Integer Parsing --
-----------------------------
parseDigit :: Parser Char
parseDigit = do 
  oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
  n <- some parseDigit
  return $ read n

-----------------------------
-- General Integer Parsing --
-----------------------------
base10Integer' :: Parser Integer
base10Integer' = do
  (base10Integer <|> (negate <$> (char '-' >> base10Integer)))

-----------------------------
-- US/CA Phone No. Parsing --
-----------------------------
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

skipNonDigits :: Parser ()
skipNonDigits = do
  skipMany (noneOf ['0'..'9'])
  

nDigits :: Int -> Parser Int 
nDigits n = do
  num <- count n digit
  return (read num)


parsePhone :: Parser PhoneNumber
parsePhone = do
  (digit >> skipNonDigits) <|> skipNonDigits
  npa <- nDigits 3
  skipNonDigits
  ex <- nDigits 3
  skipNonDigits
  ln <- nDigits 4
  return (PhoneNumber npa ex ln)

----------------
-- Log Parser --
----------------
type Year = Int
type Month = Int
type Day = Int

data Date = Date Year Month Day
  deriving (Eq, Show)

type Hours = Int
type Minutes = Int
data Time = Time Hours Minutes
  deriving (Eq, Show)
type Activity = String

data LogEntry = LogEntry Time Activity
  deriving (Eq, Show)

data DayLog = DayLog Date [LogEntry]
  deriving (Eq, Show)

type Log = [DayLog]

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- (string "--")
               skipMany (noneOf "\n")
               skipEOL)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseDate :: Parser Date
parseDate = do
  char '#' 
  skipWhitespace
  y <- nDigits 4
  char '-'
  m <- nDigits 2
  char '-'
  d <- nDigits 2
  skipMany (char ' ' <|> char '\n' <|> char '-') 
  return $ Date y m d 

parseTime :: Parser Time
parseTime = do
  hh <- decimal 
  _ <- char ':'
  mm <- decimal
  return $ Time (fromInteger hh) (fromInteger mm)

skipCmnt :: Parser ()
skipCmnt = do
  _ <- string "--"
  skipMany (noneOf "\n")
  skipEOL

readTillComment :: Parser String
readTillComment = do
  val <- manyTill anyChar ((try $ string "--") <|> string "\n") 
  return val


parseEntry :: Parser LogEntry
parseEntry = do
  time <- parseTime
  skipWhitespace
  act <- readTillComment
  return $ LogEntry time act

parseDayLog :: Parser DayLog
parseDayLog = do
  skipWhitespace
  skipComments
  date <- parseDate
  entries <- many parseEntry
  skipWhitespace
  return $ DayLog date entries

parseLog :: Parser Log
parseLog = do
  skipWhitespace
  skipComments
  days <- many parseDayLog
  return days

entryEx :: String
entryEx = [r|
08:00 Breakfast -- comment!
09:00 Sanitizing moisture collector --cmnt!
13:36 Wake up, headache
15:40 DinnerTime
|]


example :: String
example = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast -- comment!
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


