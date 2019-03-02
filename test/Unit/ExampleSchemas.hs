module Unit.ExampleSchemas (schemas) where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import Data.Function (($), (.))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (Maybe(..))

import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Text as DT

schemas :: NonEmpty (ByteString, DJJ.JsonSchema)
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
   DJJ.ObjectSchema $ DJJ.JsonObjectSchema  { DJJ.schemaRef = pure jsonSchemaDraft07SchemaRef
                                            , DJJ.idRef  = pure "http://example.com/product.schema.json"
                                            , DJJ.typeKey = pure . DJJ.TypeKey $ DJJ.One "object"
                                            , DJJ.enumKey = Nothing
                                            , DJJ.constKey = Nothing
                                            , DJJ.multipleOfKey = Nothing
                                            , DJJ.maximumKey = Nothing
                                            , DJJ.exclusiveMaximumKey = Nothing
                                            , DJJ.minimumKey = Nothing
                                            , DJJ.exclusiveMinimumKey = Nothing
                                            , DJJ.maxLengthKey = Nothing
                                            , DJJ.minLengthKey = Nothing
                                            , DJJ.patternKey = Nothing
                                            , DJJ.itemsKey = Nothing
                                            , DJJ.additionalItemsKey = Nothing
                                            , DJJ.maxItemsKey = Nothing
                }) :| []

jsonSchemaDraft07SchemaRef :: DT.Text
jsonSchemaDraft07SchemaRef = "http://json-schema.org/draft-07/schema#"
