module Mdmaster.Plugins.Registry
  ( allOperations
  ) where

import Mdmaster.Core.Operation (Operation)
import Mdmaster.Plugins.Docx (docxOperation)
import Mdmaster.Plugins.Normalize (normalizeOperation)
import Mdmaster.Plugins.StripComments (stripCommentsOperation)
import Mdmaster.Plugins.Toc (tocOperation)

allOperations :: [Operation]
allOperations =
  [ tocOperation
  , docxOperation
  , normalizeOperation
  , stripCommentsOperation
  ]
