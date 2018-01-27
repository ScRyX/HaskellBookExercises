data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i1) (TisAn i2) = i1 == i2

----------------------------------------

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2

----------------------------------------

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a1) (Hello a2) = a1 == a2
  (==) (Goodbye a1) (Goodbye a2) = a1 == a2


data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah
    | Woot deriving (Show,Eq)
settleDown x = 
    if x == Woot
    then Blah
    else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)
s1 = Sentence "dogs" "drool" -- partial application works for data constructors too!
s2 = Sentence "Julie" "loves" "dogs"


arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = i + (f a)
