module PlayingWithListMonad where

import Control.Monad

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x,x*x]
    else [x]

goodString :: String -> Maybe String
goodString s = 
  if s == "good" then Just s else Nothing

justWhatIsThis :: (Maybe Int) -> (Maybe Int)
justWhatIsThis option = do
  x <- option
  s <- if even x 
         then goodString "tralala"
         else goodString "good"
  return $ (length s) + 3
