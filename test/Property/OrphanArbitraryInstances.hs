module Property.OrphanArbitraryInstances where

import Control.Applicative ((<$>))
import Control.Monad (return)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (zip)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import Data.Tuple (uncurry)
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Property.JsonGenerators as PJG
import qualified Test.QuickCheck as TQ

instance TQ.Arbitrary a => TQ.Arbitrary (NonEmpty a) where
  arbitrary = do
    h <- TQ.arbitrary
    t <- TQ.listOf TQ.arbitrary
    return $ h :| t
  shrink = TQ.genericShrink

instance TQ.Arbitrary DT.Text where
  arbitrary = genValidUtf8
  shrink = fmap DT.pack . TQ.shrink . DT.unpack

instance TQ.Arbitrary DA.Value where
  arbitrary = PJG.valueGen PJG.defaultValueGenConfig
  shrink (DA.String t) = DA.String <$> TQ.shrink t
  shrink (DA.Number s) = DA.Number <$> f s
    where
      f :: Scientific -> [Scientific]
      f s' =
        let c = coefficient s'
            e = base10Exponent s'
         in uncurry scientific <$> zip (TQ.shrink c) (TQ.shrink e)
  shrink (DA.Bool b) = DA.Bool <$> TQ.shrink b
  shrink (DA.Object hm) = DA.Object . DHS.fromList <$> TQ.shrink (DHS.toList hm)
  shrink (DA.Array v) = DA.Array . DV.fromList <$> TQ.shrink (DV.toList v)
  shrink _ = []
