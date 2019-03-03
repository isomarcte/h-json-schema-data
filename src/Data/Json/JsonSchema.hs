module Data.Json.JsonSchema (JsonSchema(..)
                            , JsonBooleanSchema(..)
                            , JsonObjectSchema(..)
                            , TypeKey(..)
                            , OneOrSome(..)
                            ) where

import Data.Bool (Bool(..))
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.Text as DT
import Data.Tuple (swap)
import Data.Word (Word)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Map.Strict as DMS

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

newtype JsonBooleanSchema = JsonBooleanSchema Bool deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

data JsonObjectSchema where
  JsonObjectSchema :: { schemaRef :: Maybe DT.Text
                      , idRef :: Maybe DT.Text
                      , typeKey :: Maybe TypeKey
                      , enumKey :: Maybe [DA.Value]
                      , constKey :: Maybe DA.Value
                      , multipleOfKey :: Maybe Word
                      , maximumKey :: Maybe Int
                      , exclusiveMaximumKey :: Maybe Int
                      , minimumKey :: Maybe Int
                      , exclusiveMinimumKey :: Maybe Int
                      , maxLengthKey :: Maybe Word
                      , minLengthKey :: Maybe Word
                      , patternKey :: Maybe DT.Text
                      , itemsKey :: Maybe ItemsKey
                      , additionalItemsKey :: Maybe AdditionalItemsKey
                      , maxItemsKey :: Maybe Word
                      , minItemsKey :: Maybe Word
                      , uniqueItemsKey :: Maybe Bool
                      , containsKey :: Maybe JsonSchema
                      , maxPropertiesKey :: Maybe Word
                      , minPropertiesKey :: Maybe Word
                      , requiredKey :: Maybe [DT.Text]
                      , propertiesKey :: Maybe (DMS.Map DT.Text JsonSchema)
                      } -> JsonObjectSchema

deriving instance Eq JsonObjectSchema
deriving instance Generic JsonObjectSchema
deriving instance Show JsonObjectSchema

instance DA.FromJSON JsonObjectSchema where
  parseJSON = DA.genericParseJSON genericJsonOptions . fromJsonKeyMangle

instance DA.ToJSON JsonObjectSchema where
  toJSON = toJsonKeyMangle . DA.genericToJSON genericJsonOptions

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

newtype ItemsKey = ItemsKey (OneOrSome JsonSchema) deriving (Show, Eq, DA.ToJSON, DA.FromJSON)
newtype AdditionalItemsKey = AdditionalItemsKey (OneOrSome JsonSchema) deriving (Show, Eq, DA.ToJSON, DA.FromJSON)
newtype TypeKey = TypeKey (OneOrSome DT.Text) deriving (Show, Eq, DA.ToJSON, DA.FromJSON)

genericJsonOptions :: DA.Options
genericJsonOptions = DA.defaultOptions { DA.unwrapUnaryRecords = True }

untaggedJsonOptions :: DA.Options
untaggedJsonOptions = genericJsonOptions {DA.sumEncoding = DA.UntaggedValue}

keyRemappingsL :: NonEmpty (DT.Text, DT.Text)
keyRemappingsL =
  ("$schema", "schemaRef") :| ("$id", "idRef"):keyList
  where
    toTuple :: DT.Text -> (DT.Text, DT.Text)
    toTuple k = (k, k <> "Key")
    keyList :: [(DT.Text, DT.Text)]
    keyList = fmap toTuple ["enum", "const", "multipleOf", "maximum", "exclusiveMaximum",
                            "minimum", "exclusiveMinimum", "maxLength", "minLength", "pattern",
                            "type", "items", "additionalItems", "maxItems", "minItems", "uniqueItems",
                            "contains", "maxProperties", "minProperties", "required", "properties"
                           ]

fromKeyRemappings :: DMS.Map DT.Text DT.Text
fromKeyRemappings = DMS.fromList . toList $ keyRemappingsL

toKeyRemappings :: DMS.Map DT.Text DT.Text
toKeyRemappings = DMS.fromList . toList $ fmap swap keyRemappingsL

keyMangle :: DMS.Map DT.Text DT.Text -> DT.Text -> DT.Text
keyMangle m t = maybe t id $ DMS.lookup t m

jsonKeyMangle :: DMS.Map DT.Text DT.Text -> DA.Value -> DA.Value
jsonKeyMangle m (DA.Object hm) = DA.Object $ DHS.foldlWithKey' (f m) DHS.empty hm
  where
    f :: DMS.Map DT.Text DT.Text -> DHS.HashMap DT.Text DA.Value -> DT.Text -> DA.Value -> DHS.HashMap DT.Text DA.Value
    f m' acc k v = DHS.insert (keyMangle m' k) v acc
jsonKeyMangle _ v = v

toJsonKeyMangle :: DA.Value -> DA.Value
toJsonKeyMangle = jsonKeyMangle toKeyRemappings

fromJsonKeyMangle :: DA.Value -> DA.Value
fromJsonKeyMangle = jsonKeyMangle fromKeyRemappings

-- data TypeEnumeration = TypeInteger | TypeNull | TypeBoolean | TypeObject | TypeString | TypeNumber | TypeArray deriving (Eq, Ord)

-- instance Show TypeEnumeration where
--   show TypeNull = "null"
--   show TypeBoolean = "boolean"
--   show TypeObject = "object"
--   show TypeString = "string"
--   show TypeNumber = "number"
--   show TypeArray = "array"
--   show TypeInteger = "integer"

-- instance DA.FromJSON TypeEnumeration where
--   parseJSON (DA.String "null") = pure TypeNull
--   parseJSON (DA.String "boolean") = pure TypeBoolean
--   parseJSON (DA.String "object") = pure TypeObject
--   parseJSON (DA.String "string") = pure TypeString
--   parseJSON (DA.String "number") = pure TypeNumber
--   parseJSON (DA.String "array") = pure TypeArray
--   parseJSON (DA.String "integer") = pure TypeInteger
--   parseJSON v = DAT.typeMismatch "String" v

-- instance DA.ToJSON TypeEnumeration where
--   toJSON = DA.String . DT.pack . show
