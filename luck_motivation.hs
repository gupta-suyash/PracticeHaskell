import Test.QuickCheck


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

bst :: Int -> Int -> Tree Int -> Bool
bst low high tree =
	case tree of
		Empty -> True
		Node x l r ->
			low < x && x < high
			&& bst low x l && bst x high r


genTree :: Int -> Int -> Int -> Gen (Tree Int)
genTree size low high
	| low + 1 >= high = return Empty
	| otherwise =
		frequency [(1, return Empty),
			(size, do
				x <- choose (low + 1, high - 1)
				l <- genTree (size `div` 2) low x
				r <- genTree (size `div` 2) x high
				return (Node x l r))]

prop_bst = 
	forAll (genTree 10 0 42) $ \t ->
		(bst 0 42 t) ==> (bst 0 45 t)  

main = verboseCheck prop_bst
