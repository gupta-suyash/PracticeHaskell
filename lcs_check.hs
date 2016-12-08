import Test.QuickCheck

longest xs ys = if length xs > length ys then xs else ys
 
lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys) 
  | x == y    = x : lcs xs ys
  | otherwise = longest (lcs (x:xs) ys) (lcs xs (y:ys))


data List a = Nil | Cons a (List a) deriving Show

genStr :: Int -> Gen (List Int)
genStr ipos 
	| (ipos == 0) = return Nil
	| otherwise = 
		frequency[(1, return Nil),
			(ipos, do
				x <- choose (-100,100)
				y <- genStr (ipos-1)
				return (Cons x y))]


getList :: (List Int) -> [Int]
getList Nil = []
getList (Cons h t) = h : (getList t)


prop_lcs =
	forAll (genStr 10) $ \x -> 
			forAll (genStr 10) $ \y -> 
				let 	x' = (getList x) 
					y' = (getList y) 
					z  = (lcs x' y') in
					(not (null z)) ==> (length z <= length x')



main = verboseCheck prop_lcs
