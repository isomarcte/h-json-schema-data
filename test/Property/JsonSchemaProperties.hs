module Property.JsonSchemaProperties
  ( jsonSchemaProperties
  ) where

import Data.Function (($), (.))
import Property.JsonSchemaGenerators ()
import Test.QuickCheck ((===))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import qualified Data.Aeson as DA
import qualified Data.Json.JsonSchema as DJJ
import qualified Test.QuickCheck as TQ

jsonSchemaProperties :: TestTree
jsonSchemaProperties =
  testProperties
    "JsonSchema Property Tests"
    [("Json Schema Symmetry", jsonSchemaSymmetryPropForAll)]

jsonSchemaSymmetryPropForAll :: TQ.Property
jsonSchemaSymmetryPropForAll =
  TQ.forAllShrink TQ.arbitrary TQ.shrink jsonSchemaSymmetryProp

jsonSchemaSymmetryProp :: DJJ.JsonSchema -> TQ.Property
jsonSchemaSymmetryProp js = DA.Success js === (DA.fromJSON . DA.toJSON $ js)
