--Solutions to
--http://www.cse.chalmers.se/edu/year/2016/course/TDA452/oldexams/2017-01-sol.pdf

--3.a
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

--3.b
instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

--3.c
doubleTree :: Num a => Tree a -> Tree a
doubleTree = fmap (*2)

--4
a :: a -> (a,a)
a x = (x,x)

b :: (Num a, Ord a) => a -> a -> Bool
b x y = x < y + 1

c :: (a -> b -> c) -> (a,b) -> c
c x (y,z) = x y z

--5.a
parse :: Parser a -> String -> Maybe (a,String)
completeParse :: Parser a -> String -> Maybe a
completeParse p s = case parse p s of
    Just (a, "") -> a
    _            -> Nothing
