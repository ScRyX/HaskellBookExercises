mth x y z = x * y * z
mth2 x y = \z -> x * y * z
mth3 x = \y -> \z -> x * y * z -- Num a => a -> a -> a -> a
mth4 = \x -> \y -> \z -> x * y * z -- Integer -> Integer -> Integer -> Integer


addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
