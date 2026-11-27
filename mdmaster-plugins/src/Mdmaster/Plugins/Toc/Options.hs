module Mdmaster.Plugins.Toc.Options
  ( AnchorMode (..)
  , SkipTitle (..)
  , TocMode (..)
  , TocStyle (..)
  , TocOptions (..)
  , tocSchema
  , decodeTocOptions
  ) where

import Data.Maybe (fromMaybe)
import Mdmaster.Core.Options
  (EnumTable, FieldType (IntField), OptionField (..), OptionSchema (..)
  , OptionValues, decodeEnum, enumField, optInt)

data TocMode
  = TocUpdate
  | TocRemove
  deriving (Eq, Show)

data AnchorMode
  = AnchorGithub
  | AnchorPandoc
  | AnchorExplicit
  deriving (Eq, Show)

data SkipTitle
  = SkipAuto
  | SkipAlways
  | SkipNever
  deriving (Eq, Show)

data TocStyle
  = StyleBullets
  | StyleNumbered
  deriving (Eq, Show)

data TocOptions = TocOptions
  { tocMode      :: TocMode
  , tocDepth     :: Int
  , tocSkipTitle :: SkipTitle
  , tocAnchor    :: AnchorMode
  , tocStyle     :: TocStyle
  }
  deriving (Eq, Show)

defaultMode :: TocMode
defaultMode = TocUpdate

defaultDepth :: Int
defaultDepth = 3

defaultSkipTitle :: SkipTitle
defaultSkipTitle = SkipAuto

defaultAnchorMode :: AnchorMode
defaultAnchorMode = AnchorGithub

defaultStyle :: TocStyle
defaultStyle = StyleBullets

minDepth, maxDepth :: Int
minDepth = 1
maxDepth = 6

modeTable :: EnumTable TocMode
modeTable =
  [ (TocUpdate, "update", "Update")
  , (TocRemove, "remove", "Remove")
  ]

skipTitleTable :: EnumTable SkipTitle
skipTitleTable =
  [ (SkipAuto,   "auto",   "Auto")
  , (SkipAlways, "always", "Always")
  , (SkipNever,  "never",  "Never")
  ]

anchorModeTable :: EnumTable AnchorMode
anchorModeTable =
  [ (AnchorGithub,   "github",   "GitHub slugs")
  , (AnchorPandoc,   "pandoc",   "Pandoc identifiers")
  , (AnchorExplicit, "explicit", "Explicit anchors")
  ]

styleTable :: EnumTable TocStyle
styleTable =
  [ (StyleBullets,  "bullets",  "Bullets")
  , (StyleNumbered, "numbered", "Numbered")
  ]

tocSchema :: OptionSchema
tocSchema = OptionSchema
  [ OptionField
      { fieldKey   = "mode"
      , fieldLabel = "Mode"
      , fieldHelp  =
          "Update inserts or refreshes the table of contents. Remove deletes \
          \an existing one — the marker block and the anchors explicit mode \
          \injected — and leaves the rest of the document untouched."
      , fieldType  = enumField modeTable defaultMode
      }
  , OptionField
      { fieldKey   = "depth"
      , fieldLabel = "Depth"
      , fieldHelp  = "Deepest heading level included. 3 means #, ## and ###."
      , fieldType  = IntField minDepth maxDepth defaultDepth
      }
  , OptionField
      { fieldKey   = "skipTitle"
      , fieldLabel = "Skip title"
      , fieldHelp  =
          "Whether to omit the document title. Auto omits the first heading \
          \only when it is an H1, is the first heading, and is the only H1."
      , fieldType  = enumField skipTitleTable defaultSkipTitle
      }
  , OptionField
      { fieldKey   = "anchors"
      , fieldLabel = "Anchors"
      , fieldHelp  =
          "How entries link to headings. Explicit injects anchor tags so links \
          \resolve in any renderer; the others add no markup."
      , fieldType  = enumField anchorModeTable defaultAnchorMode
      }
  , OptionField
      { fieldKey   = "style"
      , fieldLabel = "Style"
      , fieldHelp  =
          "The list shape of the entries. Bullets is a nested unordered list. \
          \Numbered is a nested ordered list, which renderers that number \
          \nested ordered lists show as 1., 1.1-style numbering."
      , fieldType  = enumField styleTable defaultStyle
      }
  ]

decodeTocOptions :: OptionValues -> TocOptions
decodeTocOptions values = TocOptions
  { tocMode      = decodeEnum modeTable defaultMode "mode" values
  , tocDepth     = clamp (fromMaybe defaultDepth (optInt "depth" values))
  , tocSkipTitle = decodeEnum skipTitleTable defaultSkipTitle "skipTitle" values
  , tocAnchor    = decodeEnum anchorModeTable defaultAnchorMode "anchors" values
  , tocStyle     = decodeEnum styleTable defaultStyle "style" values
  }
  where
    clamp = max minDepth . min maxDepth
