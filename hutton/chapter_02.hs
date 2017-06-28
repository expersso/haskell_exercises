--Chapter 2
--1
--Done

--2
a = 2^3*4 == (2^3)*4
b = 2*3+4*5 == (2*3) + (4*5)
c = 2+3*4^5 == 2 + (3 * (4^5))

--3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

--4
last', last'' :: [a] -> a
last' = head . reverse
last'' xs = head $ (drop $ length xs - 1) xs

--5
init', init'' :: [a] -> [a]
init' = reverse . tail . reverse
init'' xs = take (length xs - 1) xs
