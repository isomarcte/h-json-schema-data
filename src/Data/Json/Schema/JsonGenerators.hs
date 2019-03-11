module Data.Json.Schema.JsonGenerators
  ( ValueGenConfig(..)
  , defaultValueGenConfig
  , valueGen
  , valueGen'
  , arrayGen
  , arrayGen'
  , objectGen
  , objectGen'
  , primitiveValueGen
  , primitiveValueGen'
  , shrinkValue
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (return)
import Control.Monad.Reader.Class (MonadReader(..), ask)
import Data.Bool (Bool(..))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Json.Schema.QuickCheckUtilities (resize', shrinkText, sized')
import Data.List (zip)
import Data.Scientific (Scientific(..), base10Exponent, coefficient, scientific)
import Data.Tuple (uncurry)
import Data.Word (Word)
import Prelude (Integral(..), div)
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Test.QuickCheck as TQ

data ValueGenConfig where
  ValueGenConfig
    :: { boolGen :: TQ.Gen Bool
       , stringGen :: TQ.Gen DT.Text
       , numberGen :: TQ.Gen Scientific}
    -> ValueGenConfig

defaultValueGenConfig :: ValueGenConfig
defaultValueGenConfig =
  ValueGenConfig
    { boolGen = TQ.arbitrary
    , stringGen = genValidUtf8
    , numberGen = scientificGen
    }

scientificGen :: TQ.Gen Scientific
scientificGen = scientific <$> TQ.arbitrary <*> TQ.arbitrary

objectGen :: ValueGenConfig -> TQ.Gen (DHS.HashMap DT.Text DA.Value)
objectGen cfg = sized' $ \w -> objectGen' w cfg

objectGen' ::
     MonadReader ValueGenConfig m
  => Word
  -> m (TQ.Gen (DHS.HashMap DT.Text DA.Value))
objectGen' 0 = pure . pure $ DHS.empty
objectGen' n = fmap DHS.fromList . TQ.listOf <$> pairGen n
  where
    pairGen ::
         MonadReader ValueGenConfig m => Word -> m (TQ.Gen (DT.Text, DA.Value))
    pairGen n' = do
      vg <- valueGen' (div n 4)
      return $ do
        key <- resize' n' genValidUtf8
        value <- vg
        return (key, value)

arrayGen :: ValueGenConfig -> TQ.Gen (DV.Vector DA.Value)
arrayGen cfg = sized' $ \w -> arrayGen' w cfg

arrayGen' ::
     MonadReader ValueGenConfig m => Word -> m (TQ.Gen (DV.Vector DA.Value))
arrayGen' 0 = pure $ pure DV.empty
arrayGen' n =
  let recursiveSize = div n 4
   in fmap (resize' n . fmap DV.fromList . TQ.listOf) (valueGen' recursiveSize)

primitiveValueGen :: ValueGenConfig -> TQ.Gen DA.Value
primitiveValueGen cfg = sized' $ \w -> primitiveValueGen' w cfg

primitiveValueGen' ::
     MonadReader ValueGenConfig m => Word -> m (TQ.Gen DA.Value)
primitiveValueGen' n = do
  vgc <- ask
  return . resize' n $
    TQ.oneof
      [ DA.Bool <$> boolGen vgc
      , DA.Number <$> numberGen vgc
      , DA.String <$> stringGen vgc
      , pure DA.Null
      ]

valueGen :: ValueGenConfig -> TQ.Gen DA.Value
valueGen cfg = sized' $ \w -> valueGen' w cfg

shrinkValue :: DA.Value -> [DA.Value]
shrinkValue (DA.String t) = DA.String <$> shrinkText t
shrinkValue (DA.Bool b) = DA.Bool <$> TQ.shrink b
shrinkValue (DA.Number s) =
  DA.Number . uncurry scientific <$>
  zip (TQ.shrink $ coefficient s) (TQ.shrink $ base10Exponent s)
shrinkValue (DA.Array v) =
  DA.Array . DV.fromList <$> TQ.shrinkList shrinkValue (DV.toList v)
shrinkValue (DA.Object hm) =
  DA.Object . DHS.fromList <$> TQ.shrinkList g (DHS.toList hm)
  where
    g :: (DT.Text, DA.Value) -> [(DT.Text, DA.Value)]
    g (t, v) = zip (shrinkText t) (shrinkValue v)
shrinkValue _ = []

valueGen' :: MonadReader ValueGenConfig m => Word -> m (TQ.Gen DA.Value)
valueGen' 0 = primitiveValueGen' 0
valueGen' n =
  let recursiveSize = div n 4
   in fmap TQ.oneof $ do
        pvg <- primitiveValueGen' n
        og <- objectGen' recursiveSize
        ag <- arrayGen' recursiveSize
        return [pvg, fmap DA.Object og, fmap DA.Array ag]
