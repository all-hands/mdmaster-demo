module Mdmaster.Plugins.StripComments
  ( stripCommentsOperation
  , runStripComments
  ) where

import qualified Data.Text as T
import Mdmaster.Core.Marker (carriesMarkerPrefix)
import Mdmaster.Core.Operation
  (InputFormat (MarkdownInput), OpError, Operation (..), OperationId (..))
import Mdmaster.Core.Options (OptionValues)
import Mdmaster.Plugins.StripComments.Options
  (KeepMarkers (..), decodeStripCommentsOptions, stripCommentsSchema)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

stripCommentsOperation :: Operation
stripCommentsOperation = Operation
  { opId          = OperationId "strip-comments"
  , opName        = "Strip Comments"
  , opDescription =
      "Remove every HTML comment from the document — the ones on their own \
      \lines and the ones tucked inside paragraphs — leaving the prose \
      \untouched. mdmaster's own marker comments are kept unless you say \
      \otherwise."
  , opInput       = MarkdownInput
  , opSchema      = stripCommentsSchema
  , opRun         = runStripComments
  }

runStripComments :: OptionValues -> Pandoc -> Either OpError Pandoc
runStripComments values doc = Right (stripComments (decodeStripCommentsOptions values) doc)

stripComments :: KeepMarkers -> Pandoc -> Pandoc
stripComments keep =
    walk (filter keptBlock :: [Block] -> [Block])
      . walk (filter keptInline :: [Inline] -> [Inline])
  where
    keptBlock block = case block of
      RawBlock (Format "html") txt -> keptTxt txt
      _                            -> True

    keptInline inline = case inline of
      RawInline (Format "html") txt -> keptTxt txt
      _                             -> True

    keptTxt txt
      | not (isComment txt) = True
      | otherwise           = case keep of
          KeepMarkersOn  -> carriesMarker txt
          KeepMarkersOff -> False

isComment :: T.Text -> Bool
isComment txt =
  let stripped = T.strip txt
   in "<!--" `T.isPrefixOf` stripped && "-->" `T.isSuffixOf` stripped

carriesMarker :: T.Text -> Bool
carriesMarker = carriesMarkerPrefix
