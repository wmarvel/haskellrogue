module Data.Random.PCG.Internal where

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
  next gen = (word32ToInt word, gen')
    where
      (word, gen') = step gen
  split gen@(PCGState state _) = (mkPCGGen state inc0, mkPCGGen state inc1)
    where
      (w0, gen0) = step gen
      (w1, gen1) = step gen0
      (w2, gen2) = step gen1
      (w3, _) = step gen2
      inc0 = fromWords w0 w1
      inc1 = fromWords w2 w3
  genRange _ = (fromIntegral minVal, fromIntegral maxVal)
    where
      minVal = minBound :: Int32
      maxVal = maxBound :: Int32

fromWords :: Word32 -> Word32 -> Word64
fromWords high low = shiftL (fromIntegral high) 32 .&. fromIntegral low

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral . (fromIntegral :: Word32 -> Int32)
{-# INLINE word32ToInt #-}

pcgShift :: Word64 -> Word32
pcgShift state = fromIntegral $ shiftR (xor state $ shiftR state 18) 27
{-# INLINE pcgShift #-}

pcgRotate :: Word64 -> Word32
pcgRotate state = fromIntegral $ shiftR state 59
{-# INLINE pcgRotate #-}

step :: PCGGen -> (Word32, PCGGen)
step (PCGState state inc) = (lhs .|. rhs, PCGState state' inc)
  where
    state' = state * 6364136223846793005 + inc
    xorshift = pcgShift state
    rot = pcgRotate state
    lhs = shiftR xorshift $ fromIntegral rot
    rhs = shiftL xorshift $ fromIntegral ((-rot) .&. 31)

addState :: PCGGen -> Word64 -> PCGGen
addState (PCGState state inc) state' = PCGState (state + state') inc

-- Initialize the generator in exactly the same way as
-- pcg_basic.c does
mkPCGGen :: Word64 -> Word64 -> PCGGen
mkPCGGen seed inc = snd $ next $ addState gen1 seed
  where
    gen0 = PCGState 0 $ shiftL inc 1 .|. 1
    (_, gen1) = next gen0
