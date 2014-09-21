{-# LANGUAGE GADTs, StandaloneDeriving #-}

module BinSearchTree where

data BinSearchTree a where
    Empty :: (Ord a) => BinSearchTree a
    NodeBT :: (Ord a) => a -> BinSearchTree a -> BinSearchTree a -> BinSearchTree a

deriving instance (Show a) => Show (BinSearchTree a)

isMember :: a -> BinSearchTree a -> Bool
isMember _ Empty  = False
isMember x (NodeBT e lt rt)
  | x < e = isMember x lt
  | x > e = isMember x rt
  | otherwise = True

insert :: a -> BinSearchTree a -> BinSearchTree a
insert x Empty = NodeBT x Empty Empty
insert x t@(NodeBT e lt rt)
  | x < e = NodeBT e (insert x lt) rt
  | x > e = NodeBT e lt (insert x rt)
  | otherwise = t

-- Exercise 2.2
isMember' :: a -> BinSearchTree a -> Bool
isMember' _ Empty = False
isMember' x t@(NodeBT e lt rt) = isMemberHelper x e t where
  isMemberHelper :: a -> a -> BinSearchTree a -> Bool
  isMemberHelper x x' Empty = x == x'
  isMemberHelper x x' (NodeBT e lt rt)
    | x < e = isMemberHelper x x' lt
    | otherwise = isMemberHelper x e rt
