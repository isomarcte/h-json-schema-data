module Data.Json.JsonSchema
  ( JsonSchema(..)
  , JsonBooleanSchema(..)
  , JsonObjectSchema(..)
  , ECMA262Regex(..)
  , Dependency(..)
  , ItemsKey(..)
  , TypeKey(..)
  , OneOrSome(..)
  , emptyJsonObjectSchema
  ) where

import Data.Bool (Bool(..))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Text as DT
import Data.Tuple (swap)
import Data.Word (Word)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Map.Strict as DMS

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
  deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

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
    :: { schemaRef :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-core.html#rfc.section.7>
       , idRef :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-core.html#id-keyword>
       , refRef :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-core.html#rfc.section.8.3>
       , typeKey :: Maybe TypeKey -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.1>
       , enumKey :: Maybe [DA.Value] -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.2>
       , constKey :: Maybe DA.Value -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.3>
       , multipleOfKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.1>
       , maximumKey :: Maybe Int -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.2>
       , exclusiveMaximumKey :: Maybe Int -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.3>
       , minimumKey :: Maybe Int -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.4>
       , exclusiveMinimumKey :: Maybe Int -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.2.5>
       , maxLengthKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.1>
       , minLengthKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.2>
       , patternKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.3.3>
       , itemsKey :: Maybe ItemsKey -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.1>
       , additionalItemsKey :: Maybe AdditionalItemsKey -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.2>
       , maxItemsKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.3>
       , minItemsKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.4>
       , uniqueItemsKey :: Maybe Bool -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.5>
       , containsKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.6>
       , maxPropertiesKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.1>
       , minPropertiesKey :: Maybe Word -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.2>
       , requiredKey :: Maybe [DT.Text] -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.3>
       , propertiesKey :: Maybe (DMS.Map DT.Text JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.4>
       , patternPropertiesKey :: Maybe (DMS.Map ECMA262Regex JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.5>
       , additionalPropertiesKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.6>
       , dependenciesKey :: Maybe (DMS.Map DT.Text Dependency) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.7>
       , propertyNamesKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.5.8>
       , ifKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.1>
       , thenKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.2>
       , elseKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.6.3>
       , allOfKey :: Maybe (NonEmpty JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.1>
       , anyOfKey :: Maybe (NonEmpty JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.2>
       , oneOfKey :: Maybe (NonEmpty JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.3>
       , notKey :: Maybe JsonSchema -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.7.4>
       , formatKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.7>
       , contentEncodingKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.8.3>
       , contentMediaTypeKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.8.4>
       , definitionsKey :: Maybe (DMS.Map DT.Text JsonSchema) -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.9>
       , titleKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.1>
       , descriptionKey :: Maybe DT.Text -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.1>
       , defaultKey :: Maybe DA.Value -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.2>
       , readOnlyKey :: Maybe Bool -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.3>
       , writeOnlyKey :: Maybe Bool -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.3>
       , examplesKey :: Maybe [DA.Value] -- ^ <https://json-schema.org/latest/json-schema-validation.html#rfc.section.10.4
       }
    -> JsonObjectSchema

deriving instance Eq JsonObjectSchema

deriving instance Generic JsonObjectSchema

deriving instance Show JsonObjectSchema

instance DA.FromJSON JsonObjectSchema where
  parseJSON = DA.genericParseJSON genericJsonOptions . fromJsonKeyMangle

instance DA.ToJSON JsonObjectSchema where
  toJSON = toJsonKeyMangle . DA.genericToJSON genericJsonOptions

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

-- | Many of the JSON schema types can be either a single JSON value,
-- such as a @string@, or a JSON array of values. This data type
-- encodes this representation.
--
-- Note, it is different than 'NonEmpty' because the 'Some' case might
-- very well be empty, though that is likely a poorly designed schema.
data OneOrSome a where
  One :: a -> OneOrSome a
  Some :: [a] -> OneOrSome a

deriving instance Show a => Show (OneOrSome a)

deriving instance Eq a => Eq (OneOrSome a)

deriving instance Generic (OneOrSome a)

instance DA.FromJSON a => DA.FromJSON (OneOrSome a) where
  parseJSON = DA.genericParseJSON untaggedJsonOptions

instance DA.ToJSON a => DA.ToJSON (OneOrSome a) where
  toJSON = DA.genericToJSON untaggedJsonOptions

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

-- | A newtype for @DT.Text@ values which /should/ represent an [ECMA
-- 262](https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf)
-- Regular Expression. This is not validated.
newtype ECMA262Regex =
  ECMA262Regex DT.Text
  deriving (Show, Eq, DA.ToJSON, DA.FromJSON, DA.ToJSONKey, DA.FromJSONKey, Ord)

-- | <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.1)>
newtype ItemsKey =
  ItemsKey (OneOrSome JsonSchema)
  deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

-- | <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.2>
newtype AdditionalItemsKey =
  AdditionalItemsKey (OneOrSome JsonSchema)
  deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

-- | <https://json-schema.org/latest/json-schema-validation.html#rfc.section.6.1.1>
newtype TypeKey =
  TypeKey (OneOrSome DT.Text)
  deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

genericJsonOptions :: DA.Options
genericJsonOptions = DA.defaultOptions {DA.unwrapUnaryRecords = True}

untaggedJsonOptions :: DA.Options
untaggedJsonOptions = genericJsonOptions {DA.sumEncoding = DA.UntaggedValue}

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
