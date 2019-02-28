module Unit.ExampleSchemas (schemas) where

import Data.Json.Schema (JsonSchema(..),
                         SchemaRef(..),
                         IdRef(..),
                         RelativeURI(..),
                         AbsoluteURI(..),
                         SchemaType(..),
                         TypeEnumeration(..),
                         parseRelativeOrAbsoluteURI
                        )
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))

import qualified URI.ByteString as UB

schemas :: NonEmpty (ByteString, JsonSchema)
schemas =
  ("{\
  \\"$schema\": \"http://json-schema.org/draft-07/schema#\",\
  \\"$id\": \"http://example.com/product.schema.json\",\
  \\"title\": \"Product\",\
  \\"description\": \"A product from Acme's catalog\",\
  \\"type\": \"object\",\
  \\"properties\": {\
  \\"productId\": {\
  \\"description\": \"The unique identifier for a product\",\
  \\"type\": \"integer\"\
  \},\
  \\"productName\": {\
  \\"description\": \"Name of the product\",\
  \\"type\": \"string\"\
  \},\
  \\"price\": {\
  \\"description\": \"The price of the product\",\
  \\"type\": \"number\",\
  \\"exclusiveMinimum\": 0\
  \},\
  \\"tags\": {\
  \\"description\": \"Tags for the product\",\
  \\"type\": \"array\",\
  \\"items\": {\
  \\"type\": \"string\"\
  \},\
  \\"minItems\": 1,\
  \\"uniqueItems\": true\
  \},\
  \\"dimensions\": {\
  \\"type\": \"object\",\
  \\"properties\": {\
  \\"length\": {\
  \\"type\": \"number\"\
  \},\
  \\"width\": {\
  \\"type\": \"number\"\
  \},\
  \\"height\": {\
  \\"type\": \"number\"\
  \}\
  \},\
  \\"required\": [ \"length\", \"width\", \"height\" ]\
  \},\
  \\"warehouseLocation\": {\
  \\"description\": \"Coordinates of the warehouse where the product is located.\",\
  \\"$ref\": \"https://example.com/geographical-location.schema.json\"\
  \}\
  \},\
  \\"required\": [ \"productId\", \"productName\", \"price\" ]}",
   ObjectSchema { schemaRef = pure jsonSchemaDraft07SchemaRef,
                  idRef  = pure . unsafeIdRefParse $ "http://example.com/product.schema.json",
                  typeKey = pure $ SchemaTypeSingle TypeObject,
                  enum = Nothing
                }) :| []

unsafeIdRefParse :: ByteString -> IdRef
unsafeIdRefParse = either RelativeIdRef AbsoluteIdRef . unsafeURIParse

unsafeURIParse :: ByteString -> Either RelativeURI AbsoluteURI
unsafeURIParse = either (error . show) id . parseRelativeOrAbsoluteURI

jsonSchemaDraft07SchemaRef :: SchemaRef
jsonSchemaDraft07SchemaRef =
  either (error . show) SchemaRef . unsafeURIParse $ "http://json-schema.org/draft-07/schema#"
