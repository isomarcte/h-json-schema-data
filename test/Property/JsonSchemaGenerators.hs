module Property.JsonSchemaGenerators
  ( jsonSchemaGen
  , jsonSchemaGen'
  , jsonObjectSchemaGen'
  , jsonBooleanSchemaGen
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (return)
import Data.Bool (Bool(..))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List.NonEmpty (fromList)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..))
import Data.Word (Word)
import Prelude (Integral(..), div)
import Property.QuickCheckUtilities (scale')
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT
import qualified Property.JsonGenerators as PJ
import qualified Property.QuickCheckUtilities as PQCU
import qualified Test.QuickCheck as TQ

oneOrSomeGen :: TQ.Gen a -> TQ.Gen (DJJ.OneOrSome a)
oneOrSomeGen a =
  let a' = scale' (`div` 2) a
   in TQ.oneof [fmap DJJ.One a', DJJ.Some <$> TQ.listOf a']

typeKeyGen :: TQ.Gen DT.Text -> TQ.Gen DJJ.TypeKey
typeKeyGen = fmap DJJ.TypeKey . oneOrSomeGen

itemsKeyGen :: TQ.Gen DJJ.JsonSchema -> TQ.Gen DJJ.ItemsKey
itemsKeyGen = fmap DJJ.ItemsKey . oneOrSomeGen

additionalItemsKeyGen :: TQ.Gen DJJ.JsonSchema -> TQ.Gen DJJ.AdditionalItemsKey
additionalItemsKeyGen = fmap DJJ.AdditionalItemsKey . oneOrSomeGen

ecma262RegexGen :: TQ.Gen DT.Text -> TQ.Gen DJJ.ECMA262Regex
ecma262RegexGen = fmap DJJ.ECMA262Regex

dependencyGen ::
     TQ.Gen DJJ.JsonSchema -> TQ.Gen DT.Text -> TQ.Gen DJJ.Dependency
dependencyGen js t =
  TQ.oneof [DJJ.DependencySchema <$> js, DJJ.DependencyArray <$> TQ.listOf t]

jsonSchemaGen :: TQ.Gen DJJ.JsonSchema
jsonSchemaGen = PQCU.sized' jsonSchemaGen'

jsonSchemaGen' :: Word -> TQ.Gen DJJ.JsonSchema
jsonSchemaGen' n =
  TQ.oneof
    [ DJJ.BooleanSchema <$> jsonBooleanSchemaGen TQ.arbitrary
    , DJJ.ObjectSchema <$> jsonObjectSchemaGen' PJ.defaultValueGenConfig n
    ]

jsonBooleanSchemaGen :: TQ.Gen Bool -> TQ.Gen DJJ.JsonBooleanSchema
jsonBooleanSchemaGen = fmap DJJ.JsonBooleanSchema

maybeGen :: TQ.Gen a -> TQ.Gen (Maybe a)
maybeGen g = TQ.oneof [Just <$> g, pure Nothing]

mapGen' :: Ord a => TQ.Gen a -> TQ.Gen b -> Word -> TQ.Gen (DMS.Map a b)
mapGen' _ _ 0 = pure DMS.empty
mapGen' a b _ =
  fmap DMS.fromList . TQ.listOf $ do
    key <- a
    value <- b
    return (key, value)

jsonObjectSchemaGen' :: PJ.ValueGenConfig -> Word -> TQ.Gen DJJ.JsonObjectSchema
jsonObjectSchemaGen' _ 0 = pure DJJ.emptyJsonObjectSchema
jsonObjectSchemaGen' cfg n =
  let recursiveSize = div n 2
      jsg = jsonSchemaGen' recursiveSize
   in do schemaRef' <- maybeGen genValidUtf8
         idRef' <- maybeGen genValidUtf8
         refRef' <- maybeGen genValidUtf8
         typeKey' <- maybeGen $ typeKeyGen genValidUtf8
         enumKey' <- maybeGen . TQ.listOf $ PJ.valueGen cfg
         constKey' <- maybeGen $ PJ.valueGen cfg
         multipleOfKey' <- TQ.arbitrary
         maximumKey' <- TQ.arbitrary
         exclusiveMaximumKey' <- TQ.arbitrary
         minimumKey' <- TQ.arbitrary
         exclusiveMinimumKey' <- TQ.arbitrary
         maxLengthKey' <- TQ.arbitrary
         minLengthKey' <- TQ.arbitrary
         patternKey' <- maybeGen genValidUtf8
         itemsKey' <- maybeGen $ itemsKeyGen jsg
         additionalItemsKey' <- maybeGen $ additionalItemsKeyGen jsg
         maxItemsKey' <- TQ.arbitrary
         minItemsKey' <- TQ.arbitrary
         uniqueItemsKey' <- TQ.arbitrary
         containsKey' <- maybeGen jsg
         maxPropertiesKey' <- TQ.arbitrary
         minPropertiesKey' <- TQ.arbitrary
         requiredKey' <- maybeGen $ TQ.listOf genValidUtf8
         propertiesKey' <- maybeGen $ mapGen' genValidUtf8 jsg recursiveSize
         patternPropertiesKey' <-
           maybeGen $ mapGen' (ecma262RegexGen genValidUtf8) jsg recursiveSize
         additionalPropertiesKey' <- maybeGen jsg
         dependenciesKey' <-
           maybeGen $
           mapGen' genValidUtf8 (dependencyGen jsg genValidUtf8) recursiveSize
         propertyNamesKey' <- maybeGen jsg
         ifKey' <- maybeGen jsg
         thenKey' <- maybeGen jsg
         elseKey' <- maybeGen jsg
         allOfKey' <- maybeGen . fmap fromList $ TQ.listOf1 jsg
         anyOfKey' <- maybeGen . fmap fromList $ TQ.listOf1 jsg
         oneOfKey' <- maybeGen . fmap fromList $ TQ.listOf1 jsg
         notKey' <- maybeGen jsg
         formatKey' <- maybeGen genValidUtf8
         contentEncodingKey' <- maybeGen genValidUtf8
         contentMediaTypeKey' <- maybeGen genValidUtf8
         definitionsKey' <- maybeGen $ mapGen' genValidUtf8 jsg recursiveSize
         titleKey' <- maybeGen genValidUtf8
         descriptionKey' <- maybeGen genValidUtf8
         defaultKey' <- maybeGen $ PJ.valueGen cfg
         readOnlyKey' <- TQ.arbitrary
         writeOnlyKey' <- TQ.arbitrary
         examplesKey' <- maybeGen $ TQ.listOf (PJ.valueGen cfg)
         return $
           DJJ.emptyJsonObjectSchema
             { DJJ.schemaRef = schemaRef'
             , DJJ.idRef = idRef'
             , DJJ.refRef = refRef'
             , DJJ.typeKey = typeKey'
             , DJJ.enumKey = enumKey'
             , DJJ.constKey = constKey'
             , DJJ.multipleOfKey = multipleOfKey'
             , DJJ.maximumKey = maximumKey'
             , DJJ.exclusiveMaximumKey = exclusiveMaximumKey'
             , DJJ.minimumKey = minimumKey'
             , DJJ.exclusiveMinimumKey = exclusiveMinimumKey'
             , DJJ.maxLengthKey = maxLengthKey'
             , DJJ.minLengthKey = minLengthKey'
             , DJJ.patternKey = patternKey'
             , DJJ.itemsKey = itemsKey'
             , DJJ.additionalItemsKey = additionalItemsKey'
             , DJJ.maxItemsKey = maxItemsKey'
             , DJJ.minItemsKey = minItemsKey'
             , DJJ.uniqueItemsKey = uniqueItemsKey'
             , DJJ.containsKey = containsKey'
             , DJJ.maxPropertiesKey = maxPropertiesKey'
             , DJJ.minPropertiesKey = minPropertiesKey'
             , DJJ.requiredKey = requiredKey'
             , DJJ.propertiesKey = propertiesKey'
             , DJJ.patternPropertiesKey = patternPropertiesKey'
             , DJJ.additionalPropertiesKey = additionalPropertiesKey'
             , DJJ.dependenciesKey = dependenciesKey'
             , DJJ.propertyNamesKey = propertyNamesKey'
             , DJJ.ifKey = ifKey'
             , DJJ.thenKey = thenKey'
             , DJJ.elseKey = elseKey'
             , DJJ.allOfKey = allOfKey'
             , DJJ.anyOfKey = anyOfKey'
             , DJJ.oneOfKey = oneOfKey'
             , DJJ.notKey = notKey'
             , DJJ.formatKey = formatKey'
             , DJJ.contentEncodingKey = contentEncodingKey'
             , DJJ.contentMediaTypeKey = contentMediaTypeKey'
             , DJJ.definitionsKey = definitionsKey'
             , DJJ.titleKey = titleKey'
             , DJJ.descriptionKey = descriptionKey'
             , DJJ.defaultKey = defaultKey'
             , DJJ.readOnlyKey = readOnlyKey'
             , DJJ.writeOnlyKey = writeOnlyKey'
             , DJJ.examplesKey = examplesKey'
             }
