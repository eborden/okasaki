data Tree a = Node
  { rank :: Int
  , root :: a
  , children :: [Tree a]
  }

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert []

instance Show a => Show (Tree a) where
  show = strAndDepth 0
   where
     strAndDepth depth (Node r x cs) =
       mconcat (replicate depth "| ") <> "Node " <> show r <> " " <> show x <> "\n" <>
        concatMap (strAndDepth (depth + 1)) cs

type Heap a = [Tree a]

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2 = Node (r + 1) x1 (t2 : c1)
  | otherwise = Node (r + 1) x2 (t1 : c2)

mkRank0 :: a -> Tree a
mkRank0 x = Node 0 x []

insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree x [] = [x]
insertTree x xs@(y : ys)
  | rank x < rank y = x : xs
  | otherwise = insertTree (link x y) ys

insert :: Ord a => a -> Heap a -> Heap a
insert = insertTree . mkRank0

merge :: Ord a => Heap a -> Heap a -> Heap a
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys')
  | rank x < rank y = x : merge xs' ys
  | rank y < rank x = y : merge xs ys'
  | otherwise = insertTree (link x y) (merge xs' ys')

removeMinTree :: Ord a => Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree [x] = Just (x, [])
removeMinTree (x : xs) = case removeMinTree xs of
  Nothing -> Nothing
  Just (y, ys)
    | root x <= root y -> Just (x, xs)
    | otherwise -> Just (y, x : ys)

findMin :: Ord a => Heap a -> Maybe a
findMin = fmap (root . fst) . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin xs = case removeMinTree xs of
  Nothing -> []
  Just (Node _ _ ys, zs) -> merge (reverse ys) zs
