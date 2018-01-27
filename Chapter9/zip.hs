zip :: [a] -> [b] -> [(a,b)]
zip a b = [(x,y) | x <- a, y <- b] 

zipW :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW f a b = [f x y | x <- a, y <- b]

myZip :: [a] -> [b] -> [(a,b)]
myZip a b = zipW (\x -> \y -> (x,y)) a b
