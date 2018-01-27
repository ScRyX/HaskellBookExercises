sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

printInc2 n = let plusTwo = n + 2
              in print plusTwo

test        = x  * 3 + y where x = 3; y = 1000

waxOn = x * 5 where z = 7; y = z + 8; x = y ^ 2

