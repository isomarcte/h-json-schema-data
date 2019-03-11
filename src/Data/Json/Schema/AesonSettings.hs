module Data.Json.Schema.AesonSettings
  ( genericJsonOptions
  , untaggedJsonOptions
  ) where

import Data.Bool (Bool(..))

import qualified Data.Aeson as DA

genericJsonOptions :: DA.Options
genericJsonOptions =
  DA.defaultOptions
    { DA.unwrapUnaryRecords = True
    , DA.omitNothingFields = True
    , DA.allNullaryToStringTag = False
    }

untaggedJsonOptions :: DA.Options
untaggedJsonOptions = genericJsonOptions {DA.sumEncoding = DA.UntaggedValue}
