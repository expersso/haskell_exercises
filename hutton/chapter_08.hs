--Chapter 8 - Declaring types and classes
--1
data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

--2
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Leaf 4) 5 (Leaf 6)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
    LT -> occurs' x l
    EQ -> True
    GT -> occurs' x r

--3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

t' :: Tree' Int
t' = Node' 
    (Node' (Leaf' 1) (Leaf' 2)) 
    (Node' 
       (Node' (Leaf' 3) (Leaf' 4)) 
       (Node' (Leaf' 5) (Leaf' 6))
    )

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l + leaves r) <= 1
                      && balanced l && balanced r

--4
split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r)
    where (l,r) = split xs

--5
data Expr = Val Int | Add Expr Expr deriving Show

e1 :: Expr
e1 = Add (Add (Val 1) (Val 4)) (Val 2)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

--6
eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

--7
-- instance Eq a => Eq (Maybe a) where
--     (Just x) == (Just y) = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- instance Eq a => Eq [a] where
--     [] == [] = True
--     (x:xs) == (y:ys) = x == y && xs == ys
--     _xs == _ys = False

--8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Equiv (And (Var 'A') (Not (Var 'A'))) (And (Not (Var 'A')) (Var 'A'))

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const x)   = x
eval s (Var x)     = find' x s
eval s (Not x)     = not (eval s x)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> String
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True:) bss ++ map (False:) bss
    where bss = bools (n-1)

substs :: Prop -> [Subst]
substs ps = map (zip vs) (bools (length vs))
    where vs = rmdups $ vars ps

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

