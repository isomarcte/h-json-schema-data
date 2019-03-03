module Unit.ExampleSchemas (schemas) where

import Control.Applicative (Alternative(..), Applicative(..))
import Data.ByteString (ByteString)
import Data.Function (($), (.))
import Data.List.NonEmpty (NonEmpty(..))

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
                                            , DJJ.enumKey = empty
                                            , DJJ.constKey = empty
                                            , DJJ.multipleOfKey = empty
                                            , DJJ.maximumKey = empty
                                            , DJJ.exclusiveMaximumKey = empty
                                            , DJJ.minimumKey = empty
                                            , DJJ.exclusiveMinimumKey = empty
                                            , DJJ.maxLengthKey = empty
                                            , DJJ.minLengthKey = empty
                                            , DJJ.patternKey = empty
                                            , DJJ.itemsKey = empty
                                            , DJJ.additionalItemsKey = empty
                                            , DJJ.maxItemsKey = empty
                                            , DJJ.minItemsKey = empty
                                            , DJJ.uniqueItemsKey = empty
                                            , DJJ.containsKey = empty
                                            , DJJ.maxPropertiesKey = empty
                                            , DJJ.minPropertiesKey = empty
                                            , DJJ.requiredKey = empty
                                            , DJJ.propertiesKey = empty
                }) :| []

jsonSchemaDraft07SchemaRef :: DT.Text
jsonSchemaDraft07SchemaRef = "http://json-schema.org/draft-07/schema#"
