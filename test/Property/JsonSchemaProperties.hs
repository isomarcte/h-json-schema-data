module Property.JsonSchemaProperties
  ( jsonSchemaProperties
  ) where

import Data.Function (($), (.))
import Test.QuickCheck (Property, (===), forAll)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import qualified Data.Aeson as DA
import qualified Data.Json.JsonSchema as DJJ
import qualified Property.JsonSchemaGenerators as PJSG

jsonSchemaProperties :: TestTree
jsonSchemaProperties =
  testProperties
    "JsonSchema Property Tests"
    [("Json Schema Symmetry", jsonSchemaSymmetryPropForAll)]

jsonSchemaSymmetryPropForAll :: Property
jsonSchemaSymmetryPropForAll = forAll PJSG.jsonSchemaGen jsonSchemaSymmetryProp

jsonSchemaSymmetryProp :: DJJ.JsonSchema -> Property
jsonSchemaSymmetryProp js = DA.Success js === (DA.fromJSON . DA.toJSON $ js)
