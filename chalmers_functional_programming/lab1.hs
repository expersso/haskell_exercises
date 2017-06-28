--http://www.cse.chalmers.se/edu/year/2016/course/TDA452/labs/1/

-- Part 1
-- Computing power n k takes k + 1 computation steps.
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power:negative argument"
power _ 0 = 1
power n k = n * power n (k - 1)

--2
power' :: Integer -> Integer -> Integer
power' n k = product $ replicate (fromInteger k) (fromInteger n)

--3
power'' :: Integer -> Integer -> Integer
power'' n 0 = 1
power'' n k 
    | even k = power (n * n) (k `div` 2)
    | otherwise = n * power n (k - 1)

prop_powers :: Integer -> Integer -> Bool
prop_powers n k | n < 0 || k < 0 = True
prop_powers n k = power n k == power' n k && power' n k == power'' n k

test_powers = and [prop_powers n k | n <- [0..10], k <- [0..10]]

