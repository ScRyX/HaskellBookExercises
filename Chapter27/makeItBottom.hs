module MakeItBottomOut where

x = undefined
y = x `seq` "blah"
main = do
  print (snd (x, y))
