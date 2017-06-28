-- Chapter 15

-- 6
-- Find where two consequtive values in a list differ 
-- by less than d and return just that value. If no
-- such two consequtive values exist, return Nothing.
findDiff :: (Ord a, Num a) => a -> [a] -> Maybe a
findDiff _ []  = Nothing
findDiff _ [x] = Nothing
findDiff d (x1:x2:xs) 
    | abs (x1-x2) <= d = Just x2 
    | otherwise        = findDiff d (x2:xs)

-- General sqroot
sqroot' :: (Fractional a, Ord a) => a -> a -> a -> a
sqroot' d init n = case approx of
    Just x -> x
    _      -> 0 
    where 
        approx = findDiff d $ iterate next init
        next a = (a + n/a) / 2

-- Simplified sqroot, requested in question
sqroot :: Double -> Double
sqroot = sqroot' 0.00001 1.0
