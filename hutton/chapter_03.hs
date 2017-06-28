--Chapter 3
--1
xs = ['a','b','c'] :: [Char]
ys = ('a','b','c') :: (Char, Char, Char)
zs = [(False,'O'),(True,'1')] :: [(Bool,Char)]
fs = [tail, init, reverse] :: [[a] -> [a]]

--2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

--3
seconds :: [a] -> a
seconds xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

--4
--Done

--5
--Functions as instances of Eq is only feasible for functions with
--finite domains.
