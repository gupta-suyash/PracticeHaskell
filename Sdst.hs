module Sdst where

data SList a = Nil 
		| Cons a (SList a) deriving (Show, Read, Eq, Ord)

data MTree a = Leaf
		| Node a (MTree a) (MTree a) deriving (Show, Read, Eq, Ord)
