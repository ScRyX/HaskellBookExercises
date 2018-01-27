--f :: a -> a -> a
--f x y = x

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g


fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"


main :: IO ()
main = do
  print $ 1 + 2
  print 10



f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h n = g $ f n 

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 x = let y = f1 x in fst $ f2 y
