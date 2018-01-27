{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (i,s) = i > 42 && True 

-- NOTE : Instance for class-constrained types!
instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (x,y) = tooMany (x + y) 
