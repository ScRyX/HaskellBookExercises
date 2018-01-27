isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _      = False

lefts' :: [Either a b] -> [a]
lefts' xs = foldr f [] xs
  where f (Left x) b = x:b
        f _ b        = b


rights' :: [Either a b] -> [b]
rights' xs = foldr f [] xs
  where f (Right x) b = x:b
        f _ b        = b


partitionEithers' :: [Either a b]
                  -> ([a],[b])
partitionEithers' = (\xs -> (lefts' xs, rights' xs))


eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f e =
  case e of 
  Left _ -> Nothing
  Right b -> Just $ f b



either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' l r (Left a)  = l a
either' l r (Right b) = r b


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (Just . f) e
