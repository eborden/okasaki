data Heap a
  = E
  | T Int a (Heap a) (Heap a)
  deriving (Show, Eq)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise = T (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (singleton x)

findMin :: Ord a => Heap a -> Maybe a
findMin E = Nothing
findMin (T _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin E = Nothing
deleteMin (T _ _ a b) = Just $ merge a b

insert3_5 :: Ord a => a -> Heap a -> Heap a
insert3_5 x E = singleton x
insert3_5 x t@(T _ y a b)
  | x <= y = makeT x E t
  | otherwise = makeT y a (insert3_5 x b)

singleton :: a -> Heap a
singleton x = T 1 x E E

-- What about not using foldr?
fromList :: Ord a => [a] -> Heap a
fromList = foldr (merge . singleton) E
