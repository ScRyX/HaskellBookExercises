module Types where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

x = (+)
f xs = w `x` 1 where w = length xs

type Name = String
strId :: Name -> Int
strId x = length x
