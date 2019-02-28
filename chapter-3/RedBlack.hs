
data Color = R | B
  deriving Show

data Tree a
  = E
  | T Color (Tree a) a (Tree a)
  deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ l y r)
  | x < y = member x l
  | y < x = member x r
  | otherwise = True

red :: Tree a -> a -> Tree a -> Tree a
red a x b = T R a x b

black :: Tree a -> a -> Tree a -> Tree a
black a x b = T B a x b

insert :: Ord a => a -> Tree a -> Tree a
insert x t = let T _ a y b = ins t in T B a y b
 where
  ins E = red E x E
  ins (T c l y r)
    | x < y = balance c (ins l) y r
    | y < x = balance c l y (ins r)
    | otherwise = t

balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = red (black a x b) y (black c z d)
balance B (T R a x (T R b y c)) z d = red (black a x b) y (black c z d)
balance B a x (T R (T R b y c) z d) = red (black a x b) y (black c z d)
balance B a x (T R b y (T R c z d)) = red (black a x b) y (black c z d)
balance c a x b = T c a x b

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E
