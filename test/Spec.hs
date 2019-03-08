import Data.Function (($))
import Property.JsonSchemaProperties (jsonSchemaProperties)
import System.IO (IO)
import Test.Tasty (defaultMain, testGroup)
import Unit.ParseExamples (parseExampleTree)

main :: IO ()
main =
  defaultMain $ testGroup "All Tests" [parseExampleTree, jsonSchemaProperties]
