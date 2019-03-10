module Property.JsonSchemaProperties
  ( jsonSchemaProperties
  ) where

import Data.Function (($), (.))
import Data.Functor (fmap)
import Test.QuickCheck
  ( Arbitrary(..)
  , Property
  , (===)
  , forAllShrink
  , genericShrink
  )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import qualified Data.Aeson as DA
import qualified Data.Json.JsonSchema as DJJ
import qualified Property.JsonGenerators as PJG
import qualified Property.JsonSchemaGenerators as PJSG

jsonSchemaProperties :: TestTree
jsonSchemaProperties =
  testProperties
    "JsonSchema Property Tests"
    [("Json Schema Symmetry", jsonSchemaSymmetryPropForAll)]

jsonSchemaSymmetryPropForAll :: Property
jsonSchemaSymmetryPropForAll =
  forAllShrink PJSG.jsonSchemaGen genericShrink jsonSchemaSymmetryProp

jsonSchemaSymmetryProp :: DJJ.JsonSchema -> Property
jsonSchemaSymmetryProp js = DA.Success js === (DA.fromJSON . DA.toJSON $ js)

instance Arbitrary DJJ.JsonBooleanSchema where
  arbitrary = PJSG.jsonBooleanSchemaGen arbitrary
  shrink (DJJ.JsonBooleanSchema b) = fmap DJJ.JsonBooleanSchema $ shrink b

instance Arbitrary DJJ.JsonObjectSchema where
  arbitrary = PJSG.jsonObjectSchemaGen PJG.defaultValueGenConfig
