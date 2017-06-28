--Chapter 12
--1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

t1 :: Tree Int
t1 = Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

--2
{- instance Functor ((->) a) where -}
    {- fmap = (.) -}
    
--3

