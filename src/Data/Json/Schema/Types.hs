module Data.Json.Schema.Types
  ( Tagged
  , OneOrSome(..)
  , ECMA262Regex
  , ecma262Regex
  , SchemaRef
  , schemaRef
  , IdRef
  , idRef
  , RefRef
  , refRef
  , TypeKey
  , typeKey
  , EnumKey
  , enumKey
  , ConstKey
  , constKey
  , MultipleOfKey
  , multipleOfKey
  , MaximumKey
  , maximumKey
  , ExclusiveMaximumKey
  , exclusiveMaximumKey
  , MinimumKey
  , minimumKey
  , ExclusiveMinimumKey
  , exclusiveMinimumKey
  , MaxLengthKey
  , maxLengthKey
  , MinLengthKey
  , minLengthKey
  , PatternKey
  , patternKey
  , ItemsKey
  , itemsKey
  , AdditionalItemsKey
  , additionalItemsKey
  , MaxItemsKey
  , maxItemsKey
  , MinItemsKey
  , minItemsKey
  , UniqueItemsKey
  , uniqueItemsKey
  , ContainsKey
  , containsKey
  , MaxPropertiesKey
  , maxPropertiesKey
  , MinPropertiesKey
  , minPropertiesKey
  , RequiredKey
  , requiredKey
  , PropertiesKey
  , propertiesKey
  , PatternPropertiesKey
  , patternPropertiesKey
  , AdditionalPropertiesKey
  , additionalPropertiesKey
  , DependenciesKey
  , dependenciesKey
  , PropertyNamesKey
  , propertyNamesKey
  , IfKey
  , ifKey
  , ThenKey
  , thenKey
  , ElseKey
  , elseKey
  , AllOfKey
  , allOfKey
  , AnyOfKey
  , anyOfKey
  , OneOfKey
  , oneOfKey
  , NotKey
  , notKey
  , FormatKey
  , formatKey
  , ContentEncodingKey
  , contentEncodingKey
  , ContentMediaTypeKey
  , contentMediaTypeKey
  , DefinitionsKey
  , definitionsKey
  , TitleKey
  , titleKey
  , DescriptionKey
  , descriptionKey
  , DefaultKey
  , defaultKey
  , ReadOnlyKey
  , readOnlyKey
  , WriteOnlyKey
  , writeOnlyKey
  , ExamplesKey
  , examplesKey
  ) where

import Control.Applicative ((<$>))
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor(..))
import Data.Json.Schema.AesonSettings (untaggedJsonOptions)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import qualified Data.Aeson as DA

-- import qualified Data.Text as DT
import qualified Test.QuickCheck as TQ

newtype Tagged a b = Tagged
  { value :: b
  } deriving ( Show
             , Eq
             , Ord
             , Generic
             , DA.ToJSON
             , DA.FromJSON
             , Functor
             , DA.FromJSONKey
             , DA.ToJSONKey
             )

instance TQ.Arbitrary a => TQ.Arbitrary (Tagged b a) where
  arbitrary = Tagged <$> TQ.arbitrary
  shrink = fmap Tagged . TQ.shrink . value

-- | Many of the JSON schema types can be either a single JSON value,
-- such as a @string@, or a JSON array of values. This data type
-- encodes this representation.
--
-- Note, it is different than 'NonEmpty' because the 'Some' case might
-- very well be empty, though that is likely a poorly designed schema.
data OneOrSome a where
  One :: a -> OneOrSome a
  Some :: [a] -> OneOrSome a

deriving instance Show a => Show (OneOrSome a)

deriving instance Eq a => Eq (OneOrSome a)

deriving instance Generic (OneOrSome a)

instance DA.FromJSON a => DA.FromJSON (OneOrSome a) where
  parseJSON = DA.genericParseJSON untaggedJsonOptions

instance DA.ToJSON a => DA.ToJSON (OneOrSome a) where
  toJSON = DA.genericToJSON untaggedJsonOptions

instance (TQ.Arbitrary a) => TQ.Arbitrary (OneOrSome a) where
  arbitrary = TQ.oneof [One <$> TQ.arbitrary, Some <$> TQ.listOf TQ.arbitrary]
  shrink (One a) = One <$> TQ.shrink a
  shrink (Some as) = Some <$> TQ.shrink as

-- Empty Data Constructors --
data ECMA262Regex

ecma262Regex :: a -> Tagged ECMA262Regex a
ecma262Regex = Tagged

data SchemaRef

schemaRef :: a -> Tagged SchemaRef a
schemaRef = Tagged

data IdRef

idRef :: a -> Tagged IdRef a
idRef = Tagged

data RefRef

refRef :: a -> Tagged RefRef a
refRef = Tagged

data TypeKey

typeKey :: a -> Tagged TypeKey a
typeKey = Tagged

data EnumKey

enumKey :: a -> Tagged EnumKey a
enumKey = Tagged

data ConstKey

constKey :: a -> Tagged ConstKey a
constKey = Tagged

data MultipleOfKey

multipleOfKey :: a -> Tagged MultipleOfKey a
multipleOfKey = Tagged

data MaximumKey

maximumKey :: a -> Tagged MaximumKey a
maximumKey = Tagged

data ExclusiveMaximumKey

exclusiveMaximumKey :: a -> Tagged ExclusiveMaximumKey a
exclusiveMaximumKey = Tagged

data MinimumKey

minimumKey :: a -> Tagged MinimumKey a
minimumKey = Tagged

data ExclusiveMinimumKey

exclusiveMinimumKey :: a -> Tagged ExclusiveMinimumKey a
exclusiveMinimumKey = Tagged

data MaxLengthKey

maxLengthKey :: a -> Tagged MaxLengthKey a
maxLengthKey = Tagged

data MinLengthKey

minLengthKey :: a -> Tagged MinLengthKey a
minLengthKey = Tagged

data PatternKey

patternKey :: a -> Tagged PatternKey a
patternKey = Tagged

data ItemsKey

itemsKey :: a -> Tagged ItemsKey a
itemsKey = Tagged

data AdditionalItemsKey

additionalItemsKey :: a -> Tagged AdditionalItemsKey a
additionalItemsKey = Tagged

data MaxItemsKey

maxItemsKey :: a -> Tagged MaxItemsKey a
maxItemsKey = Tagged

data MinItemsKey

minItemsKey :: a -> Tagged MinItemsKey a
minItemsKey = Tagged

data UniqueItemsKey

uniqueItemsKey :: a -> Tagged UniqueItemsKey a
uniqueItemsKey = Tagged

data ContainsKey

containsKey :: a -> Tagged ContainsKey a
containsKey = Tagged

data MaxPropertiesKey

maxPropertiesKey :: a -> Tagged MaxPropertiesKey a
maxPropertiesKey = Tagged

data MinPropertiesKey

minPropertiesKey :: a -> Tagged MinPropertiesKey a
minPropertiesKey = Tagged

data RequiredKey

requiredKey :: a -> Tagged RequiredKey a
requiredKey = Tagged

data PropertiesKey

propertiesKey :: a -> Tagged PropertiesKey a
propertiesKey = Tagged

data PatternPropertiesKey

patternPropertiesKey :: a -> Tagged PatternPropertiesKey a
patternPropertiesKey = Tagged

data AdditionalPropertiesKey

additionalPropertiesKey :: a -> Tagged AdditionalPropertiesKey a
additionalPropertiesKey = Tagged

data DependenciesKey

dependenciesKey :: a -> Tagged DependenciesKey a
dependenciesKey = Tagged

data PropertyNamesKey

propertyNamesKey :: a -> Tagged PropertyNamesKey a
propertyNamesKey = Tagged

data IfKey

ifKey :: a -> Tagged IfKey a
ifKey = Tagged

data ThenKey

thenKey :: a -> Tagged ThenKey a
thenKey = Tagged

data ElseKey

elseKey :: a -> Tagged ElseKey a
elseKey = Tagged

data AllOfKey

allOfKey :: a -> Tagged AllOfKey a
allOfKey = Tagged

data AnyOfKey

anyOfKey :: a -> Tagged AnyOfKey a
anyOfKey = Tagged

data OneOfKey

oneOfKey :: a -> Tagged OneOfKey a
oneOfKey = Tagged

data NotKey

notKey :: a -> Tagged NotKey a
notKey = Tagged

data FormatKey

formatKey :: a -> Tagged FormatKey a
formatKey = Tagged

data ContentEncodingKey

contentEncodingKey :: a -> Tagged ContentEncodingKey a
contentEncodingKey = Tagged

data ContentMediaTypeKey

contentMediaTypeKey :: a -> Tagged ContentMediaTypeKey a
contentMediaTypeKey = Tagged

data DefinitionsKey

definitionsKey :: a -> Tagged DefinitionsKey a
definitionsKey = Tagged

data TitleKey

titleKey :: a -> Tagged TitleKey a
titleKey = Tagged

data DescriptionKey

descriptionKey :: a -> Tagged DescriptionKey a
descriptionKey = Tagged

data DefaultKey

defaultKey :: a -> Tagged DefaultKey a
defaultKey = Tagged

data ReadOnlyKey

readOnlyKey :: a -> Tagged ReadOnlyKey a
readOnlyKey = Tagged

data WriteOnlyKey

writeOnlyKey :: a -> Tagged WriteOnlyKey a
writeOnlyKey = Tagged

data ExamplesKey

examplesKey :: a -> Tagged ExamplesKey a
examplesKey = Tagged
