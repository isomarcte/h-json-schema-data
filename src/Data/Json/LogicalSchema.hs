module Data.Json.LogicalSchema (SchemaRef(..)
                               , IdRef(..)
                               , RelativeURI(..)
                               , AbsoluteURI(..)
                               ) where

import Data.Eq (Eq)
import Text.Show (Show)

import qualified Data.Aeson as DA
import qualified Data.Set.NonEmpty as DSN
import qualified URI.ByteString as UB

newtype RelativeURI = RelativeURI (UB.URIRef UB.Relative) deriving (Show, Eq)
newtype AbsoluteURI = AbsoluteURI (UB.URIRef UB.Absolute) deriving (Show, Eq)
newtype SchemaRef = SchemaRef AbsoluteURI deriving (Show, Eq)
newtype JsonSchemaEnum = JsonSchemaEnum (DSN.NESet DA.Value) deriving (Show, Eq)

-- data JsonSchema where
--   BooleanSchema :: Bool -> JsonSchema
--   JsonSchemaObject :: ObjectSchema -> JsonSchema

-- data ObjectSchema where
--   ObjectSchema :: { schemaRef :: Maybe SchemaRef,
--                     idRef :: Maybe IdRef,
--                     typeKey :: Maybe SchemaType,
--                     enum :: Maybe JsonSchemaEnum
--                   } -> ObjectSchema

-- deriving instance Eq ObjectSchema
-- deriving instance Show ObjectSchema

-- deriving instance Eq JsonSchema
-- deriving instance Show JsonSchema

-- data TypeEnumeration = TypeInteger | TypeNull | TypeBoolean | TypeObject | TypeString | TypeNumber | TypeArray deriving (Eq, Ord)

-- listToNonEmptySet :: Ord a => [a] -> Maybe (DSN.NESet a)
-- listToNonEmptySet [] = Nothing
-- listToNonEmptySet (x:xs) = Just . DSN.fromList $ x :| xs

-- instance DA.FromJSON JsonSchemaEnum where
--   parseJSON = undefined
--   -- parseJSON = parseJSON >>= maybe (fail "enum must have at least one value") listToNonEmptySet

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
--   parseJSON v = typeMismatch "String" v

data IdRef = RelativeIdRef RelativeURI | AbsoluteIdRef AbsoluteURI deriving (Show, Eq)
-- data SchemaType = SchemaTypeSingle TypeEnumeration | SchemaTypeSet (DS.Set TypeEnumeration) deriving (Show, Eq, Generic)

-- instance DA.FromJSON SchemaType where
--   parseJSON = DA.genericParseJSON DA.defaultOptions{ DA.unwrapUnaryRecords = True,
--                                                      DA.sumEncoding = DA.UntaggedValue
--                                                    }

-- instance DA.FromJSON SchemaRef where
--   parseJSON (DA.String t) = either (fail . show) (pure . SchemaRef . AbsoluteURI) $
--     UB.parseURI UB.strictURIParserOptions $
--     DTE.encodeUtf8 t
--   parseJSON v = typeMismatch "String" v

-- instance DA.FromJSON IdRef where
--   parseJSON (DA.String t) =
--     either (fail . show) (pure . either RelativeIdRef AbsoluteIdRef) . parseRelativeOrAbsoluteURI $ DTE.encodeUtf8 t
--   parseJSON v = typeMismatch "String" v

-- deriving instance Generic JsonSchema
-- instance DA.FromJSON JsonSchema where
--   parseJSON v = DA.genericParseJSON DA.defaultOptions{
--     DA.unwrapUnaryRecords = True,
--     DA.sumEncoding = DA.UntaggedValue
--     } (fromJsonValueMangle v)

-- parseRelativeOrAbsoluteURI :: DB.ByteString -> Either UB.URIParseError (Either RelativeURI AbsoluteURI)
-- parseRelativeOrAbsoluteURI b =
--   case (UB.parseURI UB.strictURIParserOptions b) of
--     Right result -> Right . Right . AbsoluteURI $ result
--     _ -> case (UB.parseRelativeRef UB.strictURIParserOptions b) of
--       Right result -> Right . Left . RelativeURI $ result
--       Left err -> Left err
