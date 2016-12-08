--import Data.Random.Distribution.Normal
--import Data.Random.RVar
--import Data.Random.Sample
--
--import Data.Random.Distribution.Uniform
--import Data.Random.Distribution.Bernoulli

import System.Random.MWC.Distributions
import System.Random.MWC
import Control.Monad.ST


--outputdist :: StdGen -> (Int, StdGen) --RVar Double
--outputdist g = do
--	--x <- (normal 10 2)
--	lognorm <- (sampleState (uniform 1 100)) g
--	return lognorm
--

--outputDist :: Double
--outputDist = boolBernoulliCDF 0.5 True


--outputDist :: RVar Bool
--outputDist = do
--	x <- bernoulli 0.6
--	return x


main = do
  gen <- create
  bits <- (bernoulli 0.25 gen) 
  print bits



--main :: IO ()
--main = do
--	print outputdist
