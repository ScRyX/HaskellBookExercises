import Data.List

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:(myIterate f (f a))


myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b = case f b of 
                Nothing -> []
                Just (x,y) -> x:(myUnfoldr f y)


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y,f y)) x


-- Tree unfold o_O
data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f a = case f a of
                 Nothing -> Leaf
                 Just (x1,z,x2) -> Node (unfoldTree f x1) z (unfoldTree f x2)


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\x -> if x < n then Just (x+1,x,x+1) else Nothing) 0
