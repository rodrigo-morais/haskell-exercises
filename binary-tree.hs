{-# LANGUAGE DatatypeContexts #-}

module BST
( Tree(..)
, build
, build2
, build3
, insert
, find
, findSub
, contains
, size
, height
, delete
) where

data (Ord a, Eq a) => Tree a = Empty | Node a (Tree a) (Tree a)
  deriving Show


empty :: Tree a -> Bool
empty Empty = True
empty _ = False


build' :: (Ord a, Eq a) => [a] -> Tree a -> Tree a
build' [] tree = tree
build' (x:xs) tree = build' xs $ insert tree x

build :: (Ord a, Eq a) => [a] -> Tree a
build xs = build' xs Empty


build2 :: (Ord a) => [a] -> Tree a
build2 [] = Empty
build2 xs = build2' xs Empty
  where
    build2' (y:ys) Empty = build2' ys $ insert Empty y
    build2' (y:ys) tree = build2' ys $ insert tree y


build3 :: (Ord a) => [a] -> Tree a -> Tree a
build3 [] tree = tree
build3 (x:xs) tree = build3 xs $ insert tree x


singleNode :: (Ord a) => a -> Tree a
singleNode x = Node x Empty Empty


insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = singleNode x 
insert (Node a left right) x
  | a >= x = Node a (insert left x) right
  | otherwise = Node a left (insert right x)


findSub :: (Ord a) => Tree a -> a -> Tree a
findSub Empty _ = Empty
findSub (Node a left right) x
  | a >= x = find left x
  | otherwise = find right x


find :: (Ord a) => Tree a -> a -> Tree a
find Empty _ = Empty
find (Node a left right) x
  | a == x = Node a left right
  | a >= x = find left x
  | otherwise = find right x


contains :: (Ord a) => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node a left right)
  | a > x = contains x left
  | a < x = contains x right
  | otherwise = True


size :: (Ord a) => Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + (size left) + (size right)


height :: (Ord a) => Tree a -> Int
height Empty = 0
height (Node _ left right) = (+) 1 $ max (height left) (height right) 


findSmallest :: (Ord a) => Tree a -> a
findSmallest Empty = error "Empty list"
findSmallest (Node a left _)
  | empty left = a
  | otherwise = findSmallest left


delete :: (Ord a) => Tree a -> a -> Tree a
delete Empty _ = Empty
delete (Node a left right) x
  | a == x && empty right = left 
  | a == x && empty left = right
  | a == x = Node smaller left newRight
  | a >= x = Node a (delete left x) right
  | otherwise = Node a left (delete right x)
  where smaller = findSmallest right
        newRight = delete right smaller
