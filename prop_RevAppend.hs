import Test.QuickCheck

prop_RevAppend :: [Int] -> [Int] -> Bool
prop_RevAppend xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

--main = quickCheck prop_RevAppend
main = verboseCheck prop_RevAppend
