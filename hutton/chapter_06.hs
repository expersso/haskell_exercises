-- Chapter 6 - Recursive functions
--1
fact :: Int -> Int
fact 0 = 1
fact n 
    | n < 0 = error "Factorial only defined for positive integers"
    | otherwise = n * fact (n-1)

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3
{- (^) :: Int -> Int -> Int -}
{- x ^ 0 = 1 -}
{- x ^ n = x * (x ^ (n-1)) -}

--4
euclid :: Int -> Int -> Int
euclid p q = if r == 0 then q else euclid q r
    where r = p `mod` q

--6
--a
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and xs

--b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

--c
replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = x : replicate'' (n-1) x

--d
{- (!!) :: [a] -> Int -> a -}
{- (x:_) !! 0 = x -}
{- (_:xs) !! n = xs !! (n-1) -}

--e
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

--7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
 
--8
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort first) (msort second)
    where (first,second) = halve xs

--9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last xs


