module Mdmaster.Plugins.Normalize.Options
  ( Smart (..)
  , normalizeSchema
  , decodeNormalizeOptions
  ) where

import Mdmaster.Core.Options
  (EnumTable, OptionField (..), OptionSchema (..), OptionValues, decodeEnum
  , enumField)

data Smart
  = SmartOff
  | SmartOn
  deriving (Eq, Show)

defaultSmart :: Smart
defaultSmart = SmartOff

smartTable :: EnumTable Smart
smartTable =
  [ (SmartOff, "off", "Off")
  , (SmartOn,  "on",  "On")
  ]

normalizeSchema :: OptionSchema
normalizeSchema = OptionSchema
  [ OptionField
      { fieldKey   = "smart"
      , fieldLabel = "Smart punctuation"
      , fieldHelp  =
          "When On, straight quotes become curly, -- becomes an en dash, \
          \--- an em dash and ... an ellipsis. Off leaves the document's own \
          \typography untouched."
      , fieldType  = enumField smartTable defaultSmart
      }
  ]

decodeNormalizeOptions :: OptionValues -> Smart
decodeNormalizeOptions = decodeEnum smartTable defaultSmart "smart"
