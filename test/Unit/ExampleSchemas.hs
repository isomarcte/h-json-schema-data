module Unit.ExampleSchemas
  ( TestComparison(..)
  , schemas
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (foldM, return)
import Data.Bool (Bool(..))
import Data.Either (Either, either)
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.String (String)
import Data.Traversable (traverse)
import GHC.Err (error)
import System.FilePath.Posix (takeFileName)
import System.IO (FilePath, IO)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Json.Schema.Types as DJST
import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT
import qualified Paths_h_json_schema_data as Paths

data TestComparison a = TestComparison
  { actual :: a
  , expected :: a
  } deriving (Show, Eq)

schemaFilePaths :: IO (NonEmpty FilePath)
schemaFilePaths =
  traverse Paths.getDataFileName $
  fmap ("data/test/example/" <>) ["schema0.json"]

schemaFileMap :: IO (DMS.Map FilePath DJJ.JsonSchema)
schemaFileMap = do
  sfps <- schemaFilePaths
  foldM f DMS.empty sfps
  where
    f :: DMS.Map FilePath DJJ.JsonSchema
      -> FilePath
      -> IO (DMS.Map FilePath DJJ.JsonSchema)
    f acc value = do
      mjs <-
        DA.eitherDecodeFileStrict' value :: IO (Either String DJJ.JsonSchema)
      let js =
            either
              (\e ->
                 error $
                 "Unable to decode JsonSchema in file: " <> value <> "\nError: " <>
                 e)
              id
              mjs
      return $ DMS.insert (takeFileName value) js acc

lookupOrError :: (Ord a, Show a) => a -> DMS.Map a b -> b
lookupOrError a m =
  case DMS.lookup a m of
    Just v -> v
    _ ->
      error $
      "Unable to find " <> show a <> " in map\nMap Keys" <> show (DMS.keysSet m)

emptyWithTypeKey :: DT.Text -> DJJ.JsonSchema
emptyWithTypeKey t =
  DJJ.ObjectSchema $
  DJJ.emptyJsonObjectSchema {DJJ.typeKey = pure . DJST.typeKey $ DJST.One t}

schemas :: IO (NonEmpty (TestComparison DJJ.JsonSchema))
schemas = fmap f schemaFileMap
  where
    f :: DMS.Map FilePath DJJ.JsonSchema
      -> NonEmpty (TestComparison DJJ.JsonSchema)
    f m =
      TestComparison
        { actual = lookupOrError "schema0.json" m
        , expected =
            DJJ.ObjectSchema $
            DJJ.emptyJsonObjectSchema
              { DJJ.schemaRef =
                  pure . DJST.schemaRef $ jsonSchemaDraft07SchemaRef
              , DJJ.idRef =
                  pure $ DJST.idRef "http://example.com/product.schema.json"
              , DJJ.typeKey = pure . DJST.typeKey $ DJST.One "object"
              , DJJ.titleKey = pure $ DJST.titleKey "Product"
              , DJJ.descriptionKey =
                  pure $ DJST.descriptionKey "A product from Acme's catalog"
              , DJJ.propertiesKey =
                  pure . DJST.propertiesKey . DMS.fromList $
                  [ ( "productId"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.descriptionKey =
                            pure $
                            DJST.descriptionKey
                              "The unique identifier for a product"
                        , DJJ.typeKey = pure . DJST.typeKey $ DJST.One "integer"
                        })
                  , ( "productName"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.descriptionKey =
                            pure $ DJST.descriptionKey "Name of the product"
                        , DJJ.typeKey = pure . DJST.typeKey $ DJST.One "string"
                        })
                  , ( "price"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.descriptionKey =
                            pure $
                            DJST.descriptionKey "The price of the product"
                        , DJJ.typeKey = pure . DJST.typeKey $ DJST.One "number"
                        , DJJ.exclusiveMinimumKey =
                            pure $ DJST.exclusiveMinimumKey 0
                        })
                  , ( "tags"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.itemsKey =
                            pure . DJST.itemsKey . DJST.One $
                            emptyWithTypeKey "string"
                        , DJJ.descriptionKey =
                            pure $ DJST.descriptionKey "Tags for the product"
                        , DJJ.typeKey = pure . DJST.typeKey $ DJST.One "array"
                        , DJJ.minItemsKey = pure $ DJST.minItemsKey 1
                        , DJJ.uniqueItemsKey = pure $ DJST.uniqueItemsKey True
                        })
                  , ( "dimensions"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.typeKey = pure . DJST.typeKey $ DJST.One "object"
                        , DJJ.propertiesKey =
                            pure . DJST.propertiesKey . DMS.fromList $
                            [ ("length", emptyWithTypeKey "number")
                            , ("width", emptyWithTypeKey "number")
                            , ("height", emptyWithTypeKey "number")
                            ]
                        , DJJ.requiredKey =
                            pure $
                            DJST.requiredKey ["length", "width", "height"]
                        })
                  , ( "warehouseLocation"
                    , DJJ.ObjectSchema $
                      DJJ.emptyJsonObjectSchema
                        { DJJ.descriptionKey =
                            pure $
                            DJST.descriptionKey
                              "Coordinates of the warehouse where the product is located."
                        , DJJ.refRef =
                            pure $
                            DJST.refRef
                              "https://example.com/geographical-location.schema.json"
                        })
                  ]
              , DJJ.requiredKey =
                  pure $ DJST.requiredKey ["productId", "productName", "price"]
              }
        } :|
      []

jsonSchemaDraft07SchemaRef :: DT.Text
jsonSchemaDraft07SchemaRef = "http://json-schema.org/draft-07/schema#"
