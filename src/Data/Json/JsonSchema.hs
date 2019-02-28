module Data.Json.JsonSchema () where

import Control.Applicative (pure)
import Data.Bool (Bool(..))
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (Maybe, maybe)
import Data.Ord (Ord)
import Data.Text as DT
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DAT
import qualified Data.HashMap.Strict as DHS
import qualified Data.Map.Strict as DMS

data JsonObjectSchema where
  JsonObjectSchema :: { schemaRef :: Maybe DT.Text
                      , idRef :: Maybe DT.Text
                      , typeKey :: Maybe TypeKey
                      , enumKey :: Maybe [DA.Value]
                      } -> JsonObjectSchema

deriving instance Eq JsonObjectSchema
deriving instance Generic JsonObjectSchema
deriving instance Show JsonObjectSchema

instance DA.FromJSON JsonObjectSchema where
  parseJSON = DA.genericParseJSON genericJsonOptions . fromJsonKeyMangle

instance DA.ToJSON JsonObjectSchema where
  toJSON = toJsonKeyMangle . DA.genericToJSON genericJsonOptions

data TypeEnumeration = TypeInteger | TypeNull | TypeBoolean | TypeObject | TypeString | TypeNumber | TypeArray deriving (Eq, Ord)

instance Show TypeEnumeration where
  show TypeNull = "null"
  show TypeBoolean = "boolean"
  show TypeObject = "object"
  show TypeString = "string"
  show TypeNumber = "number"
  show TypeArray = "array"
  show TypeInteger = "integer"

instance DA.FromJSON TypeEnumeration where
  parseJSON (DA.String "null") = pure TypeNull
  parseJSON (DA.String "boolean") = pure TypeBoolean
  parseJSON (DA.String "object") = pure TypeObject
  parseJSON (DA.String "string") = pure TypeString
  parseJSON (DA.String "number") = pure TypeNumber
  parseJSON (DA.String "array") = pure TypeArray
  parseJSON (DA.String "integer") = pure TypeInteger
  parseJSON v = DAT.typeMismatch "String" v

instance DA.ToJSON TypeEnumeration where
  toJSON = DA.String . DT.pack . show

data TypeKey = TypeKeyString DT.Text | TypeKeyArray [TypeEnumeration] deriving (Show, Eq, Generic)

instance DA.FromJSON TypeKey where
  parseJSON = DA.genericParseJSON untaggedJsonOptions

instance DA.ToJSON TypeKey where
  toJSON = DA.genericToJSON untaggedJsonOptions

genericJsonOptions :: DA.Options
genericJsonOptions = DA.defaultOptions { DA.unwrapUnaryRecords = True }

untaggedJsonOptions :: DA.Options
untaggedJsonOptions = genericJsonOptions {DA.sumEncoding = DA.UntaggedValue}

keyRemappingsL :: NonEmpty (DT.Text, DT.Text)
keyRemappingsL =
  ("$schema", "schemaRef") :| [("$id", "idRef"), ("type", "typeKey")]

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
