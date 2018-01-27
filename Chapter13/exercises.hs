import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

cleanUp :: String -> String
cleanUp = map toLower . filter isLetter

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let line1 = cleanUp line
  case (line1 == reverse line1) of
    True -> do 
      putStrLn "Congrats, it's a palindrome!"
      exitSuccess
    False -> putStrLn "Try again!"

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Gimme name:"
  name <- getLine
  putStrLn "Gimme age:"
  ageStr <- getLine
  let age = read ageStr :: Integer
  let p = mkPerson name age
  case p of
    Left e -> putStrLn $ "Error: " ++ (show e)
    Right p -> putStrLn (show p)
