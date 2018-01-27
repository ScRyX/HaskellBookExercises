-- Multiple choice
-- 1D, 2B, 3D, 4B, 5A, 


-- Let's write code
-- TASK 1
tensDigit :: Integral a => a -> a
tensDigit x = d
    where dm x = (`divMod` x)  
          d = snd . dm 10 $ fst . dm 10 $ x

hundDigit :: Integral a => a -> a
hundDigit x = d
    where dm x = (`divMod` x)  
          d = snd . dm 10 $ fst . dm 100 $ x


-- TASK 2
foldBool :: a -> a -> Bool -> a
foldBool x y b 
  | b == False = x
  | b == True = y 
  -- | otherwise  = y -- b == True tu NESLO!

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b =
    case b of
      True -> y
      False -> x


-- TASK 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)


-- TASK 5
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- TASK 6
-- make roundTrip work with signature:
-- (Show a, Read b) => a -> b
-- print ((roundTrip 4) :: Integer)

