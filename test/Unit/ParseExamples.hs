module Unit.ParseExamples (parseExampleTree) where

import Control.Monad ((>>), foldM, return)
import Control.Applicative (pure)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Either (Either(..), either)
import Data.Function (($), (.), id, const)
import Data.List.NonEmpty (NonEmpty)
import Data.String (String)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertFailure, assertEqual)
import Unit.ExampleSchemas (TestComparison(..), schemas)
import System.IO (IO)

import qualified Data.Json.JsonSchema as DJJ

parseExampleTree :: TestTree
parseExampleTree =
  testGroup "Example Schema Tests" . pure . testCase "Parse Example Schemas" $ f schemas
  where
    f :: IO (NonEmpty (TestComparison DJJ.JsonSchema)) -> Assertion
    f s = do
      ne <- s
      foldM (const g) () ne
    g :: TestComparison DJJ.JsonSchema -> Assertion
    g = expectedParse

expectedParse :: TestComparison DJJ.JsonSchema -> Assertion
expectedParse (TestComparison a e) = either assertFailure id $ do
  d <- jsonSchemaEitherDecode . toStrict . encode $ a
  return $ assertEqual "Decodes to expectedValue" e a >> assertEqual "Decode is stable" a d

jsonSchemaEitherDecode :: ByteString -> Either String DJJ.JsonSchema
jsonSchemaEitherDecode = eitherDecodeStrict'
