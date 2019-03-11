module Data.Json.JsonSchema
  ( JsonSchema(..)
  , JsonBooleanSchema(..)
  , JsonObjectSchema(..)
  , Dependency(..)
  , emptyJsonObjectSchema
  ) where

import Control.Applicative ((<$>))
import Control.Monad (return)
import Data.Aeson.Types (Parser)
import Data.Bool (Bool(..))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Json.Schema.AesonSettings (genericJsonOptions, untaggedJsonOptions)
import Data.Json.Schema.QuickCheckUtilities (sized')
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Text as DT
import Data.Tuple (swap)
import Data.Word (Word)
import GHC.Generics (Generic)
import Prelude (undefined)
import Test.QuickCheck.Utf8 (genValidUtf8)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Json.Schema.JsonGenerators as DJSJ
import qualified Data.Json.Schema.Types as DJST
import qualified Data.Map.Strict as DMS
import qualified Test.QuickCheck as TQ

-- | 'JsonSchema' defines a data type for draft 7 JSON schema values.
--
-- It aims to only provide structural validation that a JSON value is
-- a valid instance of a JSON schema. That is to say, it does not do
-- any validation that the JSON primitives (string, boolean, null, and
-- number) represent valid JSON schema values. Nor does it validate
-- that JSON arrays which should represent non-empty values or sets
-- actually do so. Semantic validation of that sort is performed by
-- other types in this package.
--
-- See <https://json-schema.org/latest/json-schema-core.html#rfc.section.4.3.1>
data JsonSchema where
  BooleanSchema :: JsonBooleanSchema -> JsonSchema
  ObjectSchema :: JsonObjectSchema -> JsonSchema

deriving instance Show JsonSchema

deriving instance Eq JsonSchema

deriving instance Generic JsonSchema

instance DA.FromJSON JsonSchema where
  parseJSON = DA.genericParseJSON untaggedJsonOptions

instance DA.ToJSON JsonSchema where
  toJSON = DA.genericToJSON untaggedJsonOptions

instance TQ.Arbitrary JsonSchema where
  arbitrary =
    TQ.oneof [BooleanSchema <$> TQ.arbitrary, ObjectSchema <$> TQ.arbitrary]
  shrink (BooleanSchema b) = BooleanSchema <$> TQ.shrink b
  shrink (ObjectSchema o) = ObjectSchema <$> TQ.shrink o

-- | 'JsonBooleanSchema' is a newtype for JSON boolean value
-- representing a JSON schema. For the uninitiated, the JSON scheme
-- specification permits two types of JSON schema values. The one most
-- commonly thought of when referring to JSON schema is the
-- 'JsonObjectSchema'. The second and less well known type is the JSON
-- boolean value. A value of false will fail validation for all
-- instances, a value of true will pass validation for all instances.
--
-- See <https://json-schema.org/latest/json-schema-core.html#rfc.section.4.3.1>
newtype JsonBooleanSchema =
  JsonBooleanSchema Bool
  deriving (Generic, Show, Eq, DA.ToJSON, DA.FromJSON, TQ.Arbitrary)

-- | 'JsonObjectSchema' represents a JSON object schema structure. As
-- mentioned in 'JsonSchema', it only represents a structurally valid
-- JSON Schema instance, not a semantically valid instance.
--
-- The properties defined for JSON schema do not play nicely with
-- Haskell's reserved keywords or valid symbol naming syntax. For this
-- reason JSON schema properties which begin with a @$@ have the @$@
-- stripped and suffix @Ref@ appended. For instance, @{"$schema":
-- "http://foo.bar"}@ becomes @JsonObjectSchema {schemaRef =
-- "http://foo.bar"}@.
--
-- All JSON schema properties which are not prefixed by @$@ have the
-- suffix @Key@ appended in their Haskell representation. This is to
-- avoid conflicts with certain reserved words, e.g. @type@.
data JsonObjectSchema where
  JsonObjectSchema
    :: { schemaRef :: Maybe (DJST.Tagged DJST.SchemaRef DT.Text) -- ^ <https://json-schema.org/latest/json-schema-core.html#rfc.section.7>
       , idRef :: Maybe (DJST.Tagged DJST.IdRef DT.Text) -- ^ <https://json-schema.org/latest/json-schema-core.html#id-keyword>
       , refRef :: Maybe (DJST.Tagged DJST.RefRef DT.Text) -- ^ <https://json-schema.org/latest/json-schema-core.html#rfc.section.8.3>
       , typeKey :: Maybe (DJST.Tagged DJST.TypeKey (DJST.OneOrSome DT.Text)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.1>
       , enumKey :: Maybe (DJST.Tagged DJST.EnumKey [DA.Value]) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.2>
       , constKey :: Maybe (DJST.Tagged DJST.ConstKey DA.Value) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.3>
       , multipleOfKey :: Maybe (DJST.Tagged DJST.MultipleOfKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.1>
       , maximumKey :: Maybe (DJST.Tagged DJST.MaximumKey Int) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.2>
       , exclusiveMaximumKey :: Maybe (DJST.Tagged DJST.ExclusiveMaximumKey Int) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.3>
       , minimumKey :: Maybe (DJST.Tagged DJST.MinimumKey Int) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.4>
       , exclusiveMinimumKey :: Maybe (DJST.Tagged DJST.ExclusiveMinimumKey Int) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.5>
       , maxLengthKey :: Maybe (DJST.Tagged DJST.MaxLengthKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.1>
       , minLengthKey :: Maybe (DJST.Tagged DJST.MinLengthKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.2>
       , patternKey :: Maybe (DJST.Tagged DJST.PatternKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.3>
       , itemsKey :: Maybe (DJST.Tagged DJST.ItemsKey (DJST.OneOrSome DT.Text)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.1>
       , additionalItemsKey :: Maybe (DJST.Tagged DJST.AdditionalItemsKey (DJST.OneOrSome DT.Text)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.2>
       , maxItemsKey :: Maybe (DJST.Tagged DJST.MaxItemsKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.3>
       , minItemsKey :: Maybe (DJST.Tagged DJST.MinItemsKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.4>
       , uniqueItemsKey :: Maybe (DJST.Tagged DJST.UniqueItemsKey Bool) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.5>
       , containsKey :: Maybe (DJST.Tagged DJST.ContainsKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.6>
       , maxPropertiesKey :: Maybe (DJST.Tagged DJST.MaxPropertiesKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.1>
       , minPropertiesKey :: Maybe (DJST.Tagged DJST.MinPropertiesKey Word) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.2>
       , requiredKey :: Maybe (DJST.Tagged DJST.RequiredKey [DT.Text]) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.3>
       , propertiesKey :: Maybe (DJST.Tagged DJST.PropertiesKey (DMS.Map DT.Text JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.4>
       , patternPropertiesKey :: Maybe (DJST.Tagged DJST.PatternPropertiesKey (DMS.Map (DJST.Tagged DJST.ECMA262Regex DT.Text) JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.5>
       , additionalPropertiesKey :: Maybe (DJST.Tagged DJST.AdditionalPropertiesKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.6>
       , dependenciesKey :: Maybe (DJST.Tagged DJST.DependenciesKey (DMS.Map DT.Text Dependency)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.7>
       , propertyNamesKey :: Maybe (DJST.Tagged DJST.PropertyNamesKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.8>
       , ifKey :: Maybe (DJST.Tagged DJST.IfKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.1>
       , thenKey :: Maybe (DJST.Tagged DJST.ThenKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.2>
       , elseKey :: Maybe (DJST.Tagged DJST.ElseKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.3>
       , allOfKey :: Maybe (DJST.Tagged DJST.AllOfKey (NonEmpty JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.1>
       , anyOfKey :: Maybe (DJST.Tagged DJST.AnyOfKey (NonEmpty JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.2>
       , oneOfKey :: Maybe (DJST.Tagged DJST.OneOfKey (NonEmpty JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.3>
       , notKey :: Maybe (DJST.Tagged DJST.NotKey JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.4>
       , formatKey :: Maybe (DJST.Tagged DJST.FormatKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.7>
       , contentEncodingKey :: Maybe (DJST.Tagged DJST.ContentEncodingKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.8.3>
       , contentMediaTypeKey :: Maybe (DJST.Tagged DJST.ContentMediaTypeKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.8.4>
       , definitionsKey :: Maybe (DJST.Tagged DJST.DefinitionsKey (DMS.Map DT.Text JsonSchema)) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.9>
       , titleKey :: Maybe (DJST.Tagged DJST.TitleKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.1>
       , descriptionKey :: Maybe (DJST.Tagged DJST.DescriptionKey DT.Text) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.1>
       , defaultKey :: Maybe (DJST.Tagged DJST.DefaultKey DA.Value) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.2>
       , readOnlyKey :: Maybe (DJST.Tagged DJST.ReadOnlyKey Bool) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.3>
       , writeOnlyKey :: Maybe (DJST.Tagged DJST.WriteOnlyKey Bool) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.3>
       , examplesKey :: Maybe (DJST.Tagged DJST.ExamplesKey [DA.Value]) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.4
       }
    -> JsonObjectSchema

deriving instance Eq JsonObjectSchema

deriving instance Generic JsonObjectSchema

deriving instance Show JsonObjectSchema

instance DA.FromJSON JsonObjectSchema where
  parseJSON v@(DA.Object hm) = do
    jos <- DA.genericParseJSON genericJsonOptions $ fromJsonKeyMangle v
    let f = extractNullableKey hm
    constKey' <- f "const"
    defaultKey' <- f "default"
    return $
      ammendResult
        jos
        (DJST.constKey <$> constKey')
        (DJST.defaultKey <$> defaultKey')
    where
      extractNullableKey ::
           DHS.HashMap DT.Text DA.Value -> DT.Text -> Parser (Maybe DA.Value)
      extractNullableKey = (DA..:!)
      ammendResult ::
           JsonObjectSchema
        -> Maybe (DJST.Tagged DJST.ConstKey DA.Value)
        -> Maybe (DJST.Tagged DJST.DefaultKey DA.Value)
        -> JsonObjectSchema
      ammendResult j c d = j {constKey = c, defaultKey = d}
  parseJSON v = DA.genericParseJSON genericJsonOptions $ fromJsonKeyMangle v

instance DA.ToJSON JsonObjectSchema where
  toJSON = toJsonKeyMangle . DA.genericToJSON genericJsonOptions

instance TQ.Arbitrary JsonObjectSchema where
  arbitrary = jsonObjectSchemaGen DJSJ.defaultValueGenConfig
  shrink = TQ.genericShrink

-- | An empty 'JsonObjectSchema'. Modify this value to create the
-- schema you desire in Haskell code.
emptyJsonObjectSchema :: JsonObjectSchema
emptyJsonObjectSchema =
  JsonObjectSchema
    { schemaRef = Nothing
    , idRef = Nothing
    , refRef = Nothing
    , typeKey = Nothing
    , enumKey = Nothing
    , constKey = Nothing
    , multipleOfKey = Nothing
    , maximumKey = Nothing
    , exclusiveMaximumKey = Nothing
    , minimumKey = Nothing
    , exclusiveMinimumKey = Nothing
    , maxLengthKey = Nothing
    , minLengthKey = Nothing
    , patternKey = Nothing
    , itemsKey = Nothing
    , additionalItemsKey = Nothing
    , maxItemsKey = Nothing
    , minItemsKey = Nothing
    , uniqueItemsKey = Nothing
    , containsKey = Nothing
    , maxPropertiesKey = Nothing
    , minPropertiesKey = Nothing
    , requiredKey = Nothing
    , propertiesKey = Nothing
    , patternPropertiesKey = Nothing
    , additionalPropertiesKey = Nothing
    , dependenciesKey = Nothing
    , propertyNamesKey = Nothing
    , ifKey = Nothing
    , thenKey = Nothing
    , elseKey = Nothing
    , allOfKey = Nothing
    , anyOfKey = Nothing
    , oneOfKey = Nothing
    , notKey = Nothing
    , formatKey = Nothing
    , contentEncodingKey = Nothing
    , contentMediaTypeKey = Nothing
    , definitionsKey = Nothing
    , titleKey = Nothing
    , descriptionKey = Nothing
    , defaultKey = Nothing
    , readOnlyKey = Nothing
    , writeOnlyKey = Nothing
    , examplesKey = Nothing
    }

-- | This data type represents the values of the @dependencies@
-- property. It varies from the 'OneOrSome' pattern in that the type
-- in the "One" case would be different than the type in the "Some"
-- case.
--
-- See <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.7>
data Dependency where
  DependencySchema :: JsonSchema -> Dependency
  DependencyArray :: [DT.Text] -> Dependency

deriving instance Show Dependency

deriving instance Eq Dependency

deriving instance Generic Dependency

instance DA.ToJSON Dependency where
  toJSON = DA.genericToJSON untaggedJsonOptions

instance DA.FromJSON Dependency where
  parseJSON = DA.genericParseJSON untaggedJsonOptions

instance TQ.Arbitrary Dependency where
  arbitrary = TQ.oneof []

-- | A 'NonEmpty' list of pairs which are used to transform the JSON
-- keys in encoding and decoding to work around conflicts between the
-- JSON schema property names and Haskell syntax.
keyRemappingsL :: NonEmpty (DT.Text, DT.Text)
keyRemappingsL = refList <> keyList
  where
    toTuple :: DT.Text -> (DT.Text, DT.Text)
    toTuple k = (k, k <> "Key")
    refList :: NonEmpty (DT.Text, DT.Text)
    refList = ("$schema", "schemaRef") :| [("$id", "idRef"), ("$ref", "refRef")]
    keyList :: NonEmpty (DT.Text, DT.Text)
    keyList =
      fmap toTuple $
      "enum" :|
      [ "const"
      , "multipleOf"
      , "maximum"
      , "exclusiveMaximum"
      , "minimum"
      , "exclusiveMinimum"
      , "maxLength"
      , "minLength"
      , "pattern"
      , "type"
      , "items"
      , "additionalItems"
      , "maxItems"
      , "minItems"
      , "uniqueItems"
      , "contains"
      , "maxProperties"
      , "minProperties"
      , "required"
      , "properties"
      , "patternProperties"
      , "dependencies"
      , "propertyNames"
      , "if"
      , "then"
      , "else"
      , "allOf"
      , "anyOf"
      , "oneOf"
      , "not"
      , "format"
      , "contentEncoding"
      , "contentMediaType"
      , "definitions"
      , "title"
      , "description"
      , "default"
      , "readOnly"
      , "writeOnly"
      , "examples"
      ]

fromKeyRemappings :: DMS.Map DT.Text DT.Text
fromKeyRemappings = DMS.fromList . toList $ keyRemappingsL

toKeyRemappings :: DMS.Map DT.Text DT.Text
toKeyRemappings = DMS.fromList . toList $ fmap swap keyRemappingsL

keyMangle :: DMS.Map DT.Text DT.Text -> DT.Text -> DT.Text
keyMangle m t = fromMaybe t $ DMS.lookup t m

jsonKeyMangle :: DMS.Map DT.Text DT.Text -> DA.Value -> DA.Value
jsonKeyMangle m (DA.Object hm) =
  DA.Object $ DHS.foldlWithKey' (f m) DHS.empty hm
  where
    f :: DMS.Map DT.Text DT.Text
      -> DHS.HashMap DT.Text DA.Value
      -> DT.Text
      -> DA.Value
      -> DHS.HashMap DT.Text DA.Value
    f m' acc k v = DHS.insert (keyMangle m' k) v acc
jsonKeyMangle _ v = v

toJsonKeyMangle :: DA.Value -> DA.Value
toJsonKeyMangle = jsonKeyMangle toKeyRemappings

fromJsonKeyMangle :: DA.Value -> DA.Value
fromJsonKeyMangle = jsonKeyMangle fromKeyRemappings

oneOrSomeGen :: TQ.Gen a -> TQ.Gen (DJST.OneOrSome a)
oneOrSomeGen a =
  let a' = scale' (`div` 4) a
   in TQ.oneof [fmap DJST.One a', DJST.Some <$> TQ.listOf a']

dependencyGen :: TQ.Gen JsonSchema -> TQ.Gen DT.Text -> TQ.Gen Dependency
dependencyGen js t =
  TQ.oneof [DependencySchema <$> js, DependencyArray <$> TQ.listOf t]

jsonSchemaGen :: TQ.Gen JsonSchema
jsonSchemaGen = sized' jsonSchemaGen'

jsonSchemaGen' :: Word -> TQ.Gen JsonSchema
jsonSchemaGen' n =
  TQ.oneof
    [ BooleanSchema <$> jsonBooleanSchemaGen TQ.arbitrary
    , ObjectSchema <$> jsonObjectSchemaGen' defaultValueGenConfig n
    ]

jsonBooleanSchemaGen :: TQ.Gen Bool -> TQ.Gen JsonBooleanSchema
jsonBooleanSchemaGen = fmap JsonBooleanSchema

maybeGen :: TQ.Gen a -> TQ.Gen (Maybe a)
maybeGen g = TQ.oneof [Just <$> g, pure Nothing]

mapGen' :: Ord a => TQ.Gen a -> TQ.Gen b -> Word -> TQ.Gen (DMS.Map a b)
mapGen' _ _ 0 = pure DMS.empty
mapGen' a b _ =
  fmap DMS.fromList . TQ.listOf $ do
    key <- a
    value <- b
    return (key, value)

jsonObjectSchemaGen :: DJSJ.ValueGenConfig -> TQ.Gen JsonObjectSchema
jsonObjectSchemaGen = sized' . jsonObjectSchemaGen'

jsonObjectSchemaGen' :: DJSJ.ValueGenConfig -> Word -> TQ.Gen JsonObjectSchema
jsonObjectSchemaGen' _ 0 = pure emptyJsonObjectSchema
jsonObjectSchemaGen' cfg n =
  let recursiveSize = div n 4
      jsg = jsonSchemaGen' recursiveSize
   in do schemaRef' <- TQ.arbitrary
         idRef' <- maybeGen genValidUtf8
         refRef' <- maybeGen genValidUtf8
         typeKey' <- maybeGen $ typeKeyGen genValidUtf8
         enumKey' <- maybeGen . TQ.listOf $ DJSJ.valueGen cfg
         constKey' <- maybeGen $ DJSJ.valueGen cfg
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
         defaultKey' <- maybeGen $ DJSJ.valueGen cfg
         readOnlyKey' <- TQ.arbitrary
         writeOnlyKey' <- TQ.arbitrary
         examplesKey' <- maybeGen $ TQ.listOf (DJSJ.valueGen cfg)
         return $
           emptyJsonObjectSchema
             { schemaRef = schemaRef'
             , idRef = idRef'
             , refRef = refRef'
             , typeKey = typeKey'
             , enumKey = enumKey'
             , constKey = constKey'
             , multipleOfKey = multipleOfKey'
             , maximumKey = maximumKey'
             , exclusiveMaximumKey = exclusiveMaximumKey'
             , minimumKey = minimumKey'
             , exclusiveMinimumKey = exclusiveMinimumKey'
             , maxLengthKey = maxLengthKey'
             , minLengthKey = minLengthKey'
             , patternKey = patternKey'
             , itemsKey = itemsKey'
             , additionalItemsKey = additionalItemsKey'
             , maxItemsKey = maxItemsKey'
             , minItemsKey = minItemsKey'
             , uniqueItemsKey = uniqueItemsKey'
             , containsKey = containsKey'
             , maxPropertiesKey = maxPropertiesKey'
             , minPropertiesKey = minPropertiesKey'
             , requiredKey = requiredKey'
             , propertiesKey = propertiesKey'
             , patternPropertiesKey = patternPropertiesKey'
             , additionalPropertiesKey = additionalPropertiesKey'
             , dependenciesKey = dependenciesKey'
             , propertyNamesKey = propertyNamesKey'
             , ifKey = ifKey'
             , thenKey = thenKey'
             , elseKey = elseKey'
             , allOfKey = allOfKey'
             , anyOfKey = anyOfKey'
             , oneOfKey = oneOfKey'
             , notKey = notKey'
             , formatKey = formatKey'
             , contentEncodingKey = contentEncodingKey'
             , contentMediaTypeKey = contentMediaTypeKey'
             , definitionsKey = definitionsKey'
             , titleKey = titleKey'
             , descriptionKey = descriptionKey'
             , defaultKey = defaultKey'
             , readOnlyKey = readOnlyKey'
             , writeOnlyKey = writeOnlyKey'
             , examplesKey = examplesKey'
             }
