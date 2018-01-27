dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 


add :: Int -> Int -> Int
add x y = x + y
addPF :: Int -> Int -> Int
addPF = (+)
addOne :: Int -> Int
addOne = \x -> x + 1
addOnePF :: Int -> Int
addOnePF = (+1)
