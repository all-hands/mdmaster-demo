module Mdmaster.Core.DocxImage
  ( extractedKey
  , extractedValue
  , isExtracted
  , markExtracted
  , unmarkExtracted
  ) where

import Data.Text (Text)
import Text.Pandoc.Definition (Attr)

extractedKey, extractedValue :: Text
extractedKey = "extracted"
extractedValue = "true"

isExtracted :: Attr -> Bool
isExtracted (_, _, kvs) = lookup extractedKey kvs == Just extractedValue

markExtracted :: Attr -> Attr
markExtracted (ident, classes, kvs) =
  (ident, classes, kvs <> [(extractedKey, extractedValue)])

unmarkExtracted :: Attr -> Attr
unmarkExtracted (ident, classes, kvs) =
  (ident, classes, [(k, v) | (k, v) <- kvs, k /= extractedKey])
