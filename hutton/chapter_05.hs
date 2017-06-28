-- Chapter 5 - List comprehensions
-- 1
fst_100_int_squares :: Int
fst_100_int_squares = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

--5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], 
                     b <- [1..n],
                     c <- [1..n],
                     a^2 + b^2 == c^2]

--6
divides :: Int -> Int -> Bool
divides m n
    | mod n m == 0 = True
    | otherwise = False

factors :: Int -> [Int]
factors n = [m | m <- [1..(n `div` 2)], m `divides` n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

--7
x = [(x,y) | x <- [1,2], y <- [3,4]]

--8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

--9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

