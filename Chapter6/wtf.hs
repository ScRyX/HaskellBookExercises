-- THIS DOESN'T WORK EVEN THOUGH
-- NUM IS A SUPERSET OF FRACTIONAL
-- * Could not deduce (Fractional a) arising from the literal `1.0'

f :: Num a => a
f = 1.0 -- this works with 1 (integer)

g :: Num a => a -> a -> a
g x y = x + y
