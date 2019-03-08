module Property.JsonSchemaProperties
  ( jsonSchemaProperties
  ) where

import Test.QuickCheck (Arbitrary(..), sized)
import Test.Tasty (TestTree, testGroup)

import qualified Data.Json.JsonSchema as DJJ

jsonSchemaProperties :: TestTree
jsonSchemaProperties = testGroup "JsonSchema Property Tests" []

deriving instance Arbitrary DJJ.JsonBooleanSchema
