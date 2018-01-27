module Prin3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = concat [hello, " ", world]

area d = pi * ( r * r )
    where r = d / 2

area2 d = let r = d / 2 in pi * (r * r)

rvrs = concat [drop 9 s,take 2 $ drop 6 s,take 5 s]
    where s = "Curry is awesome"
