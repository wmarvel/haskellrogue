module System.Random.PCG.Internal where

import Data.Bits
import Data.Int
import Data.Word
import Numeric (showHex)
import System.Random

data PCGGen =
  PCGState Word64
           Word64

instance Show PCGGen where
  show (PCGState state inc) =
     "PCGGen 0x" ++ showHex state (" 0x" ++ showHex inc "")

instance RandomGen PCGGen where
  next = pcgNext
  split = pcgSplit
  genRange _ = pcgRange

pcgNext :: PCGGen -> (Int, PCGGen)
pcgNext gen = (word32ToInt word, gen')
  where
    (word, gen') = step gen

pcgRange :: (Int, Int)
pcgRange = (fromIntegral rMin, fromIntegral rMax)
    where
      rMin = minBound :: Int32
      rMax = maxBound :: Int32

pcgSplit :: PCGGen -> (PCGGen, PCGGen)
pcgSplit gen = (mkPCGGen bits0 bits1, mkPCGGen bits1 bits0)
  where
    (w0, gen0) = step gen
    (w1, gen1) = step gen0
    (w2, gen2) = step gen1
    (w3, _) = step gen2
    bits0 = fromWords w0 w1
    bits1 = fromWords w2 w3

{-# INLINE fromWords #-}
fromWords :: Word32 -> Word32 -> Word64
fromWords high low = shiftL (fromIntegral high) 32 .|. fromIntegral low

{-# INLINE word32ToInt #-}
word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral . (fromIntegral :: Word32 -> Int32)

{-# INLINE pcgShift #-}
pcgShift :: Word64 -> Word32
pcgShift state = fromIntegral $ shiftR (xor state $ shiftR state 18) 27

{-# INLINE pcgRotate #-}
pcgRotate :: Word64 -> Word32
pcgRotate state = fromIntegral $ shiftR state 59

{-# INLINE pcgAddState #-}
pcgAddState :: PCGGen -> Word64 -> PCGGen
pcgAddState (PCGState state inc) state' = PCGState (state + state') inc

step :: PCGGen -> (Word32, PCGGen)
step (PCGState state inc) = (lhs .|. rhs, PCGState state' inc)
  where
    state' = state * 6364136223846793005 + inc
    xorshift = pcgShift state
    rot = pcgRotate state
    lhs = shiftR xorshift $ fromIntegral rot
    rhs = shiftL xorshift $ fromIntegral ((-rot) .&. 31)

-- Initialize the generator in exactly the same way as pcg_basic.c
mkPCGGen :: Word64 -> Word64 -> PCGGen
mkPCGGen seed inc = snd $ step $ pcgAddState gen1 seed
  where
    gen0 = PCGState 0 $ shiftL inc 1 .|. 1
    (_, gen1) = step gen0

