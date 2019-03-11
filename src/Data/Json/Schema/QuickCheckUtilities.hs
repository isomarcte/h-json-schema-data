module Data.Json.Schema.QuickCheckUtilities
  ( clampToWord
  , resize'
  , sized'
  , scale'
  ) where

import Data.Function ((.))
import Data.Ord (Ord(..))
import Data.Word (Word)
import Prelude (Integral(..), fromIntegral, otherwise)

import qualified Test.QuickCheck as TQ

clampToWord :: (Integral a, Ord a) => a -> Word
clampToWord n
  | n < 0 = 0
  | otherwise = fromIntegral n

resize' :: Word -> TQ.Gen a -> TQ.Gen a
resize' = TQ.resize . fromIntegral

sized' :: (Word -> TQ.Gen a) -> TQ.Gen a
sized' f = TQ.sized (f . clampToWord)

scale' :: (Word -> Word) -> TQ.Gen a -> TQ.Gen a
scale' f = TQ.scale (fromIntegral . f . clampToWord)
