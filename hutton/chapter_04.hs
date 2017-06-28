--Chapter 4
--1
halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

--2
--a
third :: [a] -> a
third = head . tail .tail

--b
third' :: [a] -> a
third' = (!! 2)

--c
third'' :: [a] -> a
third'' (_:_:x:xs) = x

--3
--a
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

--b
safetail' :: [a] -> [a]
safetail' xs
    | null xs = []
    | otherwise = tail xs

--c
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

--4
(||-) :: Bool -> Bool -> Bool
True  ||- True  = True
True  ||- False = True
False ||- True  = True
False ||- False = False

(|-|) :: Bool -> Bool -> Bool
False |-| False = False
_     |-| _     = True

(-||) :: Bool -> Bool -> Bool
False -|| b = b
True  -|| _ = True

(|--) :: Bool -> Bool -> Bool
b |-- c | not b && not c = False
        | otherwise      = True

--5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then if b then True else False else False

--6

--7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

--8
luhnDouble :: Int -> Int
luhnDouble n = if n2 > 9 then n2 - 9 else n2
    where n2 = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn m n o p = total `mod` 10 == 0
    where total = sum [luhnDouble m, n, luhnDouble o, p]

divides, divisibleBy :: Int -> Int -> Bool
divides m n = n `mod` m == 0
divisibleBy = flip divides
