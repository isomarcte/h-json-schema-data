module Property.JsonSchemaGenerators
  (
  ) where

import Control.Applicative (Applicative(..), (<$>), liftA)
import Control.Monad (liftM, return)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Bool (Bool(..))
import Data.Function (($), (.), flip)
import Data.Ord (Ord(..))
import Data.Scientific (Scientific(..), scientific)
import Data.Word (Word)
import Prelude (Integral(..), Num(..), div, fromIntegral, otherwise)
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Test.QuickCheck as TQ

pureGen :: Applicative f => TQ.Gen a -> TQ.Gen (f a)
pureGen = liftA pure

oneOrSomeGen :: TQ.Gen a -> TQ.Gen (DJJ.OneOrSome a)
oneOrSomeGen a = TQ.oneof [liftA DJJ.One a, liftA DJJ.Some $ TQ.listOf a]

typeKeyGen :: TQ.Gen DT.Text -> TQ.Gen DJJ.TypeKey
typeKeyGen = liftA DJJ.TypeKey . oneOrSomeGen

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

clampToWord :: (Integral a, Ord a) => a -> Word
clampToWord n
  | n < 0 = 0
  | otherwise = fromIntegral n

resize' :: Word -> TQ.Gen a -> TQ.Gen a
resize' = TQ.resize . fromIntegral

sized' :: (Word -> TQ.Gen a) -> TQ.Gen a
sized' f = TQ.sized (f . clampToWord)

scientificGen :: TQ.Gen Scientific
scientificGen = scientific <$> TQ.arbitrary <*> TQ.arbitrary

objectGen :: ValueGenConfig -> TQ.Gen (DHS.HashMap DT.Text DA.Value)
objectGen cfg = sized' $ (\w -> objectGen' w cfg)

objectGen' ::
     MonadReader ValueGenConfig m
  => Word
  -> m (TQ.Gen (DHS.HashMap DT.Text DA.Value))
objectGen' 0 = pure . pure $ DHS.empty
objectGen' n = liftM DHS.fromList . TQ.listOf $ pairGen n
  where
    pairGen ::
         MonadReader ValueGenConfig m => Word -> m (TQ.Gen (DT.Text, DA.Value))
    pairGen n' = valueGen' (div n 2) >>=
      key <- resize' n' genValidUtf8
      value <- valueGen' (div n 2)
      return (key, value)

arrayGen :: ValueGenConfig -> TQ.Gen (DV.Vector DA.Value)
arrayGen cfg = sized' $ arrayGen' cfg

arrayGen' ::
     MonadReader ValueGenConfig m => Word -> m (TQ.Gen (DV.Vector DA.Value))
arrayGen' 0 = pure DV.empty
arrayGen' n =
  let recursiveSize = div n 2
   in resize' n $ DV.fromList <$> TQ.listOf (valueGen' cfg recursiveSize)

primitiveValueGen :: ValueGenConfig -> TQ.Gen DA.Value
primitiveValueGen cfg = sized' $ primitiveValueGen' cfg

primitiveValueGen' ::
     MonadReader ValueGenConfig m => Word -> m (TQ.Gen DA.Value)
primitiveValueGen' (ValueGenConfig {boolGen, stringGen, numberGen}) n =
  resize' n $
  TQ.oneof
    [ liftA DA.Bool boolGen
    , liftA DA.Number numberGen
    , liftA DA.String stringGen
    , pure DA.Null
    ]

valueGen :: ValueGenConfig -> TQ.Gen DA.Value
valueGen cfg = sized' $ valueGen' cfg

valueGen' :: MonadReader ValueGenConfig m => Word -> m (TQ.Gen DA.Value)
valueGen' 0 = primitiveValueGen' cfg 0
valueGen' n =
  let recursiveSize = div n 2
   in TQ.oneof $
      [ primitiveValueGen' cfg n
      , DA.Object <$> objectGen' cfg recursiveSize
      , DA.Array <$> arrayGen' cfg recursiveSize
      ]
-- valueGen :: ValueGenConfig -> TQ.Gen DA.Value
-- valueGen =
-- structurallyValidJsonObjectSchemaGen :: TQ.Gen DJJ.JsonObjectSchema
-- structurallyValidJsonObjectSchemaGen =
--   DJJ.JsonObjectSchema <$>
--   pureGen genValidUtf8 <*>
--   pureGen genValidUtf8 <*>
--   pureGen genValidUtf8 <*>
