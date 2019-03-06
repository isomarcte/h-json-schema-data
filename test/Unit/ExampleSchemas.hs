module Unit.ExampleSchemas (TestComparison(..)
                           , schemas
                           ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (foldM, return)
import Data.Either (Either, either)
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.String (String)
import Data.Traversable (traverse)
import GHC.Err (error)
import System.FilePath.Posix (takeFileName)
import System.IO (IO, FilePath)
import Text.Show (Show(..))
import Data.Maybe (Maybe(..))

import qualified Data.Aeson as DA
import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT
import qualified Paths_h_json_schema_data as Paths

data TestComparison a = TestComparison {actual :: a, expected :: a} deriving (Show, Eq)

schemaFilePaths :: IO (NonEmpty FilePath)
schemaFilePaths = traverse Paths.getDataFileName $ fmap ("data/test/example/" <>) ["schema0.json"]

schemaFileMap :: IO (DMS.Map FilePath DJJ.JsonSchema)
schemaFileMap = do
  sfps <- schemaFilePaths
  foldM f DMS.empty sfps
  where
    f :: DMS.Map FilePath DJJ.JsonSchema -> FilePath -> IO (DMS.Map FilePath DJJ.JsonSchema)
    f acc value = do
      mjs <- DA.eitherDecodeFileStrict' value :: IO (Either String DJJ.JsonSchema)
      let js = either (\e -> error $ "Unable to decode JsonSchema in file: " <> value <> "\nError: " <> e) id mjs
      return $ DMS.insert (takeFileName value) js acc

lookupOrError :: (Ord a, Show a) => a -> DMS.Map a b -> b
lookupOrError a m =
  case DMS.lookup a m of
    Just v -> v
    _ -> error $ "Unable to find " <> show a <> " in map\nMap Keys" <> show (DMS.keysSet m)

schemas :: IO (NonEmpty (TestComparison DJJ.JsonSchema))
schemas =
  fmap f schemaFileMap
  where
    f :: DMS.Map FilePath DJJ.JsonSchema -> NonEmpty (TestComparison DJJ.JsonSchema)
    f m =
      TestComparison {actual = lookupOrError "schema0.json" m
                     , expected = DJJ.ObjectSchema $ DJJ.JsonObjectSchema  { DJJ.schemaRef = pure jsonSchemaDraft07SchemaRef
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
                                            }
                     } :| []

jsonSchemaDraft07SchemaRef :: DT.Text
jsonSchemaDraft07SchemaRef = "http://json-schema.org/draft-07/schema#"
