{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

data Tree (rank :: Nat) a = Node
  { root :: a
  , children :: Ranked rank a
  }
  deriving (Show)

--fromList :: Ord a => [a] -> Heap a
--fromList = foldr insert (Heap [])

newtype Heap a = Heap { unHeap :: Ranked 0 a }
  deriving newtype (Show)

data Ranked (rank :: Nat) a
  = Nil
  | forall rank2 . Cons (Tree rank a) (Ranked (rank2 > rank) a)
  deriving (Show)

link :: Ord a => Tree n a -> Tree n a -> Tree n a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2 = Node x1 (t2 `Cons` c1)
  | otherwise = Node x2 (t1 `Cons` c2)

  {-
mkRank0 :: a -> Ranked a
mkRank0 x = Ranked 0 $ Node x []

insertTree :: Ord a => Ranked a -> Heap a -> Heap a
insertTree x (Heap []) = Heap [x]
insertTree x (Heap xs@(y : ys))
  | rank x < rank y = Heap $ x : xs
  | otherwise = insertTree
    (Ranked (rank x + 1) (link (tree x) (tree y)))
    (Heap ys)

insert :: Ord a => a -> Heap a -> Heap a
insert = insertTree . mkRank0

merge :: Ord a => Heap a -> Heap a -> Heap a
merge xs (Heap []) = xs
merge (Heap []) ys = ys
merge (Heap xs@(x : xs')) (Heap ys@(y : ys'))
  | rank x < rank y = Heap $ x : merge' xs' ys
  | rank y < rank x = Heap $ y : merge' xs ys'
  | otherwise = insertTree (Ranked (rank x + 1) (link (tree x) (tree y)))
  $ Heap (merge' xs' ys')
  where merge' x y = unHeap $ merge (Heap x) (Heap y)

removeMinTree :: Ord a => Heap a -> Maybe (Ranked a, Heap a)
removeMinTree (Heap []) = Nothing
removeMinTree (Heap [x]) = Just (x, Heap [])
removeMinTree (Heap (x : xs)) = case removeMinTree $ Heap xs of
  Nothing -> Nothing
  Just (y, Heap ys)
    | root (tree x) <= root (tree y) -> Just (x, Heap xs)
    | otherwise -> Just (y, Heap $ x : ys)

findMin :: Ord a => Heap a -> Maybe a
findMin = fmap (root . tree . fst) . removeMinTree

findMin' :: Ord a => Heap a -> Maybe a
findMin' (Heap []) = Nothing
findMin' (Heap (x : xs)) = Just . root $ tree x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin xs = case removeMinTree xs of
  Nothing -> Heap []
  Just (Ranked r (Node _ ys), zs) ->
    merge (Heap $ zipWith Ranked [0 .. r - 1] $ reverse ys) zs
-}
