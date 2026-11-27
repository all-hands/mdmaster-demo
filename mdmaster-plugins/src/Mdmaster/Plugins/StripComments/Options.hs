module Mdmaster.Plugins.StripComments.Options
  ( KeepMarkers (..)
  , stripCommentsSchema
  , decodeStripCommentsOptions
  ) where

import Mdmaster.Core.Options
  (EnumTable, OptionField (..), OptionSchema (..), OptionValues, decodeEnum
  , enumField)

data KeepMarkers
  = KeepMarkersOn
  | KeepMarkersOff
  deriving (Eq, Show)

defaultKeepMarkers :: KeepMarkers
defaultKeepMarkers = KeepMarkersOn

keepMarkersTable :: EnumTable KeepMarkers
keepMarkersTable =
  [ (KeepMarkersOn,  "true",  "True")
  , (KeepMarkersOff, "false", "False")
  ]

stripCommentsSchema :: OptionSchema
stripCommentsSchema = OptionSchema
  [ OptionField
      { fieldKey   = "keepMarkers"
      , fieldLabel = "Keep mdmaster markers"
      , fieldHelp  =
          "Keep the <!-- mdmaster:... start/end --> comments that bracket \
          \generated regions such as the table of contents. True keeps them; \
          \false strips them like any other comment."
      , fieldType  = enumField keepMarkersTable defaultKeepMarkers
      }
  ]

decodeStripCommentsOptions :: OptionValues -> KeepMarkers
decodeStripCommentsOptions =
  decodeEnum keepMarkersTable defaultKeepMarkers "keepMarkers"
