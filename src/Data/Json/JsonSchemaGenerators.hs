module Data.Json.Schema.JsonSchemaGenerators
  ( jsonSchemaGen
  , jsonSchemaGen'
  , jsonObjectSchemaGen
  , jsonObjectSchemaGen'
  , jsonBooleanSchemaGen
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (return)
import Data.Bool (Bool(..))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Json.Schema.QuickCheckUtilities (scale')
import Data.List.NonEmpty (fromList)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..))
import Data.Word (Word)
import Prelude (Integral(..), div)
import Test.QuickCheck.Utf8 (genValidUtf8)

import qualified Data.Json.Schema.JsonGenerators as PJ
import qualified Data.Json.Schema.Types as DJST
import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT
import qualified Test.QuickCheck as TQ
