module Data.Json.Schema.QuickCheckUtilities
  ( clampToWord
  , resize'
  , sized'
  , scale'
  , shrinkText
  , nonEmptyGen
  , shrinkNonEmpty
  , shrinkMap
  ) where

import Control.Monad (return)
import Data.Foldable (foldl')
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (zip)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Ord (Ord(..))
import Data.Word (Word)
import Prelude (Integral(..), fromIntegral, otherwise)

import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT
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

shrinkText :: DT.Text -> [DT.Text]
shrinkText = fmap DT.pack . TQ.shrink . DT.unpack

nonEmptyGen :: TQ.Gen a -> TQ.Gen (NonEmpty a)
nonEmptyGen gen = do
  h <- gen
  t <- TQ.listOf gen
  return $ h :| t

shrinkNonEmpty :: (a -> [a]) -> NonEmpty a -> [NonEmpty a]
shrinkNonEmpty f v = foldl' g [] $ TQ.shrinkList f (toList v)
  where
    g :: [NonEmpty b] -> [b] -> [NonEmpty b]
    g acc (a:as) = (a :| as) : acc
    g acc _ = acc

shrinkMap :: (Ord a) => (a -> [a]) -> (b -> [b]) -> DMS.Map a b -> [DMS.Map a b]
shrinkMap f g = fmap DMS.fromList . h f g . DMS.toList
  where
    h :: (c -> [c]) -> (d -> [d]) -> [(c, d)] -> [[(c, d)]]
    h f' g' = fmap (\(c, d) -> zip (f' c) (g' d))
