import Test.QuickCheck

data List a = Nil | Cons a (List a) deriving Show

lengthList :: (List Int) -> Int
lengthList Nil = 0
lengthList (Cons h t) = 1 + (lengthList t)	

getElem :: (List Int) -> Int -> Int
getElem Nil pos = 0
getElem (Cons h t) pos = if (pos == 0) then h
				else (getElem t (pos-1))


genStr :: Int -> Gen (List Int)
genStr ipos 
	| (ipos == 0) = return Nil
	| otherwise = 
		frequency[(1, return Nil),
			(ipos, do
				x <- choose (0,10)
				y <- genStr (ipos-1)
				return (Cons x y))]


genSubstr :: (List Int) -> Int -> Int -> Gen (List Int)
genSubstr str len ipos  
	| len == 0 = return Nil
	| (lengthList str) < len = return Nil
	| otherwise = do
		y <- genSubstr str (len-1) (ipos+1)
		return (Cons (getElem str ipos) y)


posAndLen :: (List Int) -> Gen (Int, Int)
posAndLen str = do 
	pos <- choose (0, (lengthList(str)-1))
	len <- choose (pos, (lengthList(str)-1))
	return (pos, (len-pos+1))


prop_subst =
	forAll (genStr 10) $ \x -> 
			forAll (posAndLen x) $ \(p,l) ->
				forAll (genSubstr x l p) $ \t -> (lengthList t) <= l

main = verboseCheck prop_subst
