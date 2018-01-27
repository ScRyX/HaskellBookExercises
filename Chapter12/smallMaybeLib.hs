module SmallMaybeModule where

-- 1)
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a 

-- 3)
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a

-- 4) List to maybe? F that shit


-- 5)
catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just a) -> a) . filter isJust

-- 6)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs 
  | (length $ catMaybes xs) /= length xs = Nothing
  | otherwise = Just $ catMaybes xs 
