module Property.JsonSchemaGenerators
  (
  ) where

import Control.Applicative (Applicative(..), (<$>), liftA)
import Data.Bool (Bool(..))
import Data.Function (($), (.))
import Data.Scientific (Scientific(..), scientific)
import Data.Word (Word)
import Test.QuickCheck (Arbitrary(..), Gen, elements, listOf, oneof)
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Json.JsonSchema as DJJ
import qualified Data.Text as DT
import qualified Data.Vector as DV

pureGen :: Applicative f => Gen a -> Gen (f a)
pureGen = liftA pure

oneOrSomeGen :: Gen a -> Gen (DJJ.OneOrSome a)
oneOrSomeGen a = oneof [liftA DJJ.One a, liftA DJJ.Some $ listOf a]

typeKeyGen :: Gen DT.Text -> Gen DJJ.TypeKey
typeKeyGen = liftA DJJ.TypeKey . oneOrSomeGen

data ValueGenConfig where
  ValueGenConfig
    :: { boolGen :: Gen Bool
       , stringGen :: Gen DT.Text
       , numberGen :: Gen Scientific}
    -> ValueGenConfig

defaultValueGenConfig :: ValueGenConfig
defaultValueGenConfig =
  ValueGenConfig
    {boolGen = arbitrary, stringGen = genValidUtf8, numberGen = scientificGen}

scientificGen :: Gen Scientific
scientificGen = scientific <$> arbitrary <*> arbitrary

objectGen :: 

valueGen' :: ValueGenConfig -> Word -> Gen DA.Value
valueGen' (ValueGenConfig {boolGen, stringGen, numberGen}) 0 =
  oneof
    [ liftA DA.Bool boolGen
    , liftA DA.Number numberGen
    , liftA DA.String stringGen
    , elements $ pure DA.Null
    ]
valueGen' (ValueGenConfig {boolGen, stringGen, numberGen}) n =
  oneof
    [ liftA DA.Bool boolGen
    , liftA DA.Number numberGen
    , liftA DA.String stringGen
    , elements $ pure DA.Null
    , list
    ]
-- valueGen :: ValueGenConfig -> Gen DA.Value
-- valueGen =
-- structurallyValidJsonObjectSchemaGen :: Gen DJJ.JsonObjectSchema
-- structurallyValidJsonObjectSchemaGen =
--   DJJ.JsonObjectSchema <$>
--   pureGen genValidUtf8 <*>
--   pureGen genValidUtf8 <*>
--   pureGen genValidUtf8 <*>
