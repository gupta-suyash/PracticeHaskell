data Bool  = False | True

data Shape = Circle Float Float Float 
		| Rectangle Float Float Float Float deriving Show

surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2


data MList a = Nil 
		| Cons a (MList a) deriving (Show, Read, Eq, Ord)

data MTree a = Leaf
		| Node a (MTree a) (MTree a) deriving (Show, Read, Eq, Ord)


-- Assuming we know data structure name, its constructors, we now need to generate an instance
-- of that data structure.
-- So, possibly data-structure's inductive definition is passed as a file. 
