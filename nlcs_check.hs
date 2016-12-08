import Test.QuickCheck

longest xs ys zs = if length xs > length ys 
			then if length xs > length zs 
				then xs 
				else zs 
			else if length ys > length zs
				then ys
				else zs
 
lcs [] _ _ = []
lcs _ [] _ = []
lcs _ _ [] = []
lcs (x:xs) (y:ys) (z:zs) 
  | (x == y && y == z) = x : lcs xs ys zs
  | otherwise = longest (lcs xs (y:ys) (z:zs)) (lcs (x:xs) ys (z:zs)) (lcs (x:xs) (y:ys) zs)


data List a = Nil | Cons a (List a) deriving Show

genStr :: Int -> Gen (List Int)
genStr ipos 
	| (ipos == 0) = return Nil
	| otherwise = 
		frequency[(1, return Nil),
			(ipos, do
				x <- choose (0,9)
				y <- genStr (ipos-1)
				return (Cons x y))]


getList :: (List Int) -> [Int]
getList Nil = []
getList (Cons h t) = h : (getList t)


prop_lcs =
	forAll (genStr 8) $ \x -> 
			forAll (genStr 8) $ \y -> 
				forAll (genStr 8) $ \w ->
					let 	x' = (getList x) 
						y' = (getList y) 
						w' = (getList w)
						z  = (lcs x' y' w') in
						(not (null z)) ==> (length z <= length x')



main = verboseCheck prop_lcs
