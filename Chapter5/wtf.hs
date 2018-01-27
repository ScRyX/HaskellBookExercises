kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
-- why the hell is a also Num? 
-- Prelude> :t kessel 1 2
-- kessel 1 2 :: (Num a, Ord a) => a
--
-- SOLUTION:
-- Because the pre-filled 1 can in the most general way be Num a
