{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

data Tree a = E | T (Tree a) a (Tree a)
  deriving (Show)

empty = E

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T l y r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x t@(T l y r)
  | x < y = T (insert x l) y r
  | x > y = T l y (insert x r)
  | otherwise = t

member2_2 :: Ord a => a -> Tree a -> Bool
member2_2 x = descend Nothing
 where
  descend mayY E = maybe False (x ==) mayY
  descend mayY (T l y r)
    | x >= y = descend (Just y) r
    | otherwise = descend mayY l

insert2_3 :: Ord a => a -> Tree a -> Tree a
insert2_3 x y' = fromMaybe y' $ descend y'
 where
  descend E = Just $ T E x E
  descend t@(T l y r)
    | x < y = (\l' -> T l' y r) <$> descend l
    | x > y = T l y <$> descend r
    | otherwise = Nothing

insert2_4 :: Ord a => a -> Tree a -> Tree a
insert2_4 x y' = fromMaybe y' $ descend Nothing y'
 where
  descend mayY E = case mayY of
    Nothing -> Just $ T E x E
    Just y
      | y == x -> Nothing
      | otherwise -> Just $ T E x E
  descend mayY t@(T l y r)
    | x >= y = T l y <$> descend (Just y) r
    | otherwise = (\l' -> T l' y r) <$> descend mayY l

insert2_5 :: Ord a => a -> Tree (Int, a) -> Tree (Int, a)
insert2_5 x E = T E (1, x) E
insert2_5 x (T l (i, y) r)
  | x < y = T (insert2_5 x l) (i, y) r
  | x > y = T l (i, y) (insert2_5 x r)
  | otherwise = T l (succ i, x) r
