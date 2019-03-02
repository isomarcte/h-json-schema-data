module Unit.ParseExamples (parseExampleTree) where

import Control.Monad ((>>), return)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Either (Either(..), either)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.List.NonEmpty (toList)
import Data.String (String)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertFailure, assertEqual)
import Unit.ExampleSchemas (schemas)

import qualified Data.Json.JsonSchema as DJJ

parseExampleTree :: TestTree
parseExampleTree =
  testGroup "Example Schema Tests" . fmap (testCase "Parse Example Schema" . expectedParse) . toList $ schemas

expectedParse :: (ByteString, DJJ.JsonSchema) -> Assertion
expectedParse (b, j) = either assertFailure id $ do
  d <- jsonSchemaEitherDecode b
  d' <- jsonSchemaEitherDecode . toStrict . encode $ d
  return $ assertEqual "Decodes to expectedValue" j d >> assertEqual "Decode is stable" j d'

jsonSchemaEitherDecode :: ByteString -> Either String DJJ.JsonSchema
jsonSchemaEitherDecode = eitherDecodeStrict'
