module Unit.ParseExamples (parseExampleTree) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Json.Schema(JsonSchema(..))
import Data.List.NonEmpty (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertBool, assertFailure, assertEqual)
import URI.ByteString (URIParseError)
import Unit.ExampleSchemas (schemas)

parseExampleTree :: TestTree
parseExampleTree =
  testGroup "Example Schema Tests" . fmap (testCase "Parse Example Schema" . expectedParse) . toList $ schemas

expectedParse :: (ByteString, JsonSchema) -> Assertion
expectedParse (b, j) =
  either (assertFailure) (assertEqual "JsonSchema Parse Equality" j) . jsonschemaeitherdecode $ b

jsonschemaeitherdecode :: ByteString -> Either String JsonSchema
jsonschemaeitherdecode = eitherDecodeStrict'
