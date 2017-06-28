--Chapter 7 - Higher-order functions
--1
mapFilt :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilt f p xs = [f x | x <- xs, p x]

mapFilt' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilt' f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\x y -> x && p y) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldl (\x y -> x || p y) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x:xs

--3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int :: [Int] -> Int
dec2int = read . concatMap show

--5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y) = f x y

--6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . (!! 0)) (drop 1) 

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

--7

--8
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

--9
luhn :: [Int] -> Bool
luhn xs = total xs `mod` 10 == 0
    where total = sum . map (`mod` 9) . altMap id (*2) . reverse


