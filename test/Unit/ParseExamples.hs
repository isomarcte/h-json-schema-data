module Unit.ParseExamples (parseExampleTree) where

import Control.Monad ((>>=), (>>), foldM, return)
import Control.Applicative (pure)
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
import Unit.ExampleSchemas (TestComparison(..), schemas)
import Data.Traversable (traverse)

import qualified Data.Json.JsonSchema as DJJ

parseExampleTree :: TestTree
parseExampleTree =
  testGroup "Example Schema Tests" . toList . fmap (testCase "Parse Example Schemas") $ traverse _ schemas

expectedParse :: TestComparison DJJ.JsonSchema -> Assertion
expectedParse (TestComparison a e) = either assertFailure id $ do
  d <- jsonSchemaEitherDecode . toStrict . encode $ a
  return $ assertEqual "Decodes to expectedValue" e a >> assertEqual "Decode is stable" a d

jsonSchemaEitherDecode :: ByteString -> Either String DJJ.JsonSchema
jsonSchemaEitherDecode = eitherDecodeStrict'
