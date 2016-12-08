--import Text.Printf
--import Control.Exception
--import System.CPUTime
import System.Random
 

znum = toInteger (0)

gensortList :: (RandomGen g, RandomGen a) => Int -> Int -> Int -> Int -> g -> a -> [Int]
gensortList 0 low high last _ _ = []
gensortList n low high last generator cflip = if (toss > znum) 
						then x : (gensortList (n-1) low high (if (x>last) then x else last) newGenerator flipgen)
						else xs : (if xs < high 
								then (gensortList (n-1) xs high xs sortGenerator flipgen) 
								else (gensortList (n-1) xs (xs+high) xs sortGenerator flipgen))
			where
				(toss,flipgen) = randomR (0,1) cflip
				(x,newGenerator) = randomR (low,high) generator
				(xs,sortGenerator) = randomR (last,high) generator



main :: IO ()
main = do
	cflip <- newStdGen
	generator <- newStdGen
	print (gensortList 15 0 100 0 generator cflip)


