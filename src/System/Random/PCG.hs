module System.Random.PCG (
    mkPCGGen,
    getPCGGen,
    newPCGGen,
    getPCGRandom,
) where

import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Random.PCG.Internal

{-# NOINLINE globalGen #-}
globalGen :: IORef PCGGen
globalGen = unsafePerformIO $ do
    gen <- mkPCGGen'
    newIORef gen

-- | This is not really robust.
mkPCGGen' :: IO PCGGen
mkPCGGen' = do
    pTime <- getPOSIXTime
    mkPCGGen (floor pTime) . fromInteger <$> getCPUTime

newPCGGen :: IO PCGGen
newPCGGen = atomicModifyIORef' globalGen split

getPCGGen :: IO PCGGen
getPCGGen = readIORef globalGen

getPCGRandom :: (PCGGen -> (a, PCGGen)) -> IO a
getPCGRandom f = atomicModifyIORef' globalGen (swap . f)
  where
    swap (b, a) = (a, b)
