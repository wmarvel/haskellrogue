module Foo where

import System.Random
import Data.Bits

-- both evenRandom and oddRandom will need to somehow use an odd limit, so
-- we get a number in a range with an even number of elements, or we will
-- introduce bias into the number generated.

-- For oddRandom, we want the range to top out at an odd number, but we want
-- that odd number to be lower than the passed limit if the passed limit is
-- even, and the same as the passed limit if the passed limit is odd.
-- Restated, 'subtract one if even' - and the function
-- (.|. 1) . (subtract 1)
-- has this effect (subtract 1 if even)

-- For evenRandom, we also want the range to top out at an odd number (you
-- cannot have an even number of elements if the top of the limit is not odd).
-- However, in this case we just want  'add one if the limit is even' because
-- the function we use to get the even number is basically 'subtract one if
-- the argument is odd'. The function
-- (.|. 1)
-- has this effect (add one if even)

-- fRes is the function that turns the generated value into a result
-- fLim is the function that turns the passed limit into the real limit
maskedRandom :: (Int -> Int) -> (Int -> Int) -> Int -> IO Int
maskedRandom fRes fLim limit =
  fmap fRes $ getStdRandom $ randomR (0, fLim limit)

oddRandom :: Int -> IO Int
oddRandom = maskedRandom (.|. 1) (.|. 1) . (subtract 1)

evenRandom :: Int -> IO Int
evenRandom = maskedRandom (.&. (maxBound - 1)) (.|. 1)

