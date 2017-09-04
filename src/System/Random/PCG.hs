module System.Random.PCG
  (
    mkPCGGen
  , getPCGGen
  , newPCGGen
  , getPCGRandom
  )
  where

import System.CPUTime (getCPUTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random 
import System.Random.PCG.Internal
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

{-# NOINLINE globalGen #-}
globalGen :: IORef PCGGen
globalGen = unsafePerformIO $ do
  gen <- mkPCGGen'
  newIORef gen

-- | This is not really robust.
mkPCGGen' :: IO PCGGen
mkPCGGen' = do
  pTime <- getPOSIXTime
  sTime <- getCPUTime
  pure $ mkPCGGen (floor pTime) (fromInteger sTime)

newPCGGen :: IO PCGGen
newPCGGen = atomicModifyIORef' globalGen split

getPCGGen :: IO PCGGen
getPCGGen = readIORef globalGen

getPCGRandom :: (PCGGen -> (a, PCGGen)) -> IO a
getPCGRandom f = atomicModifyIORef' globalGen (swap . f)
  where swap (b, a) = (a, b)
