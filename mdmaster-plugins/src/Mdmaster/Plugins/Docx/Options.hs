module Mdmaster.Plugins.Docx.Options
  ( CommentStyle (..)
  , docxSchema
  , decodeDocxOptions
  ) where

import Mdmaster.Core.Options
  (EnumTable, OptionField (..), OptionSchema (..), OptionValues, decodeEnum
  , enumField)

data CommentStyle
  = DiffStable
  | Review
  | Agent
  | Archive
  deriving (Eq, Show)

defaultCommentStyle :: CommentStyle
defaultCommentStyle = Review

commentStyleTable :: EnumTable CommentStyle
commentStyleTable =
  [ (DiffStable, "diff-stable", "Diff-stable")
  , (Review,     "review",      "Review")
  , (Agent,      "agent",       "Agent")
  , (Archive,    "archive",     "Archive")
  ]

docxSchema :: OptionSchema
docxSchema = OptionSchema
  [ OptionField
      { fieldKey   = "comments"
      , fieldLabel = "Comments"
      , fieldHelp  =
          "How Word comments are written into the Markdown. Review reads best; \
          \Diff-stable leaves the prose untouched so two versions can be \
          \compared; Agent is machine-readable; Archive keeps every detail."
      , fieldType  = enumField commentStyleTable defaultCommentStyle
      }
  ]

decodeDocxOptions :: OptionValues -> CommentStyle
decodeDocxOptions =
  decodeEnum commentStyleTable defaultCommentStyle "comments"
