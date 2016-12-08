import Sdst
import System.Random


listelem :: Int -> Int -> IO Int
listelem lo hi = do
    num <- randomRIO (lo, hi)
    return num


genMList :: Int -> SList Int
genMList 0 = Nil
genMList n = Cons 5 (genMList (n - 1))


-- Use a flag to ensure that on even count Left (or right) gets priority and odd count no one creates a node.
genMTree :: Int -> MTree Int
genMTree n = if (n <= 0) then Leaf
		else Node 5 (genMTree (n-1)) (genMTree (n-2)) 

simpt :: Int -> Int -> IO ()
simpt lo hi = do
    num <- randomRIO (lo, hi)
    print $ genMList num



