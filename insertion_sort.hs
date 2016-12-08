import Text.Printf
import Control.Exception
import System.CPUTime
 
--time :: IO t -> IO t
--time a = do
--	start <- getCPUTime
--	v <- a
--	end   <- getCPUTime
--	let diff = (fromIntegral (end - start)) / (10^12)
--	printf "Computation time: %0.3f sec\n" (diff :: Double)
--	return v
 

insertionSort :: (Ord a) => [a] -> [a]
insertionSort xs = insertionSort'' xs []
		where
			insertionSort'' :: (Ord a) => [a] -> [a]-> [a]
			insertionSort'' [] sorted = sorted
			insertionSort'' (x:xs) sorted = insertionSort'' xs newSorted
				where
					newSorted = [y | y <- sorted, y <= x] ++ [x] ++ [y | y <- sorted, y>x]


main = do
	--putStrLn "Starting..."
	st <- getCPUTime
	--time $ product [1..10000] `seq` return ()
	output <- insertionSort [64,31, 34, 17, 29, 65, 15, 29, 57, 52, 48, 21, 55, 26, 3, 20, 20, 37, 39, 20, 30, 14, 51, 17, 33, 67, 21, 5, 67, 55]`seq` return ()
	end <- getCPUTime
	let diff = (fromIntegral (end - st)) / (10^12)
	printf "Computation time: %0.6f sec\n" (diff :: Double)


-- [0,1,2,3,6,8,12,14,19,22,24,45,49,68,69,72,75,79,80,83,85,87,92,95,99,101,102,103,107,109,111,112,115,118,120] -- 0.000026
-- [0,1,2,3,6,8,12,14,87,22,24,45,49,68,107,72,75,79,80,83,85,19,92,95,99,101,102,103,69,109,111,112,115,118,120] -- 0.000028
-- [68,1,2,115,6,8,12,14,87,22,24,45,49,0,107,72,75,79,80,83,85,19,92,95,99,101,102,103,69,109,111,112,3,118,120] -- 0.000035
-- [68,1,2,115,6,8,12,14,87,22,112,95,49,0,107,72,75,79,80,83,85,19,92,45,99,101,102,103,69,109,111,24,3,118,120] -- 0.000036
-- [68,102,2,115,6,8,12,14,87,22,112,95,49,0,107,72,120,79,80,83,85,19,92,45,99,101,1,103,69,109,111,24,3,118,75] -- 0.000038
-- [120,118,115,112,111,109,107,103,102,101,99,95,92,87,85,83,80,79,75,72,69,68,49,45,24,22,19,14,12,8,6,3,2,1,0] -- 0.000065
