module Mdmaster.App.Docx
  ( parseDocx
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.App.Docx.Archive (docxParts)
import Mdmaster.App.Docx.Comments (commentMetadata)
import Mdmaster.App.Docx.Media (docxMedia)
import Mdmaster.Core.CommentSpan
  (CommentMeta, commentStartClass, enrichAttr, idKey)
import Text.Pandoc
  (ReaderOptions (..), TrackChanges (AllChanges), def, getDefaultExtensions
  , getMediaBag, readDocx, renderError, runPure)
import Text.Pandoc.Definition (Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

readerOptions :: ReaderOptions
readerOptions = def
  { readerExtensions   = getDefaultExtensions "docx"
  , readerTrackChanges = AllChanges
  }

parseDocx :: BL.ByteString -> Either Text (Pandoc, Map Text ByteString)
parseDocx bytes = do
  parts <- docxParts bytes
  (doc, bag) <- case runPure (readWithBag bytes) of
    Left err -> Left (renderError err)
    Right out -> Right out
  let enriched = walk (enrich (commentMetadata parts)) doc
  pure (enriched, docxMedia enriched bag)
  where
    readWithBag b = do
      doc <- readDocx readerOptions b
      bag <- getMediaBag
      pure (doc, bag)

enrich :: Map Text CommentMeta -> Inline -> Inline
enrich metadata inline = case inline of
  Span attr@(_, classes, kvs) body
    | commentStartClass `elem` classes ->
        Span (enrichAttr attr (lookup idKey kvs >>= (`M.lookup` metadata))) body
  _ -> inline
