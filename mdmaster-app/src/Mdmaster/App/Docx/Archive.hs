module Mdmaster.App.Docx.Archive
  ( DocxParts (..)
  , docxParts
  , readArchive
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, toArchiveOrFail)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

data DocxParts = DocxParts
  { dpComments           :: Maybe ByteString
  , dpCommentsExtended   :: Maybe ByteString
  , dpCommentsIds        :: Maybe ByteString
  , dpCommentsExtensible :: Maybe ByteString
  }
  deriving (Eq, Show)

docxParts :: ByteString -> Either Text DocxParts
docxParts bytes = do
  archive <- readArchive bytes
  let part path = fromEntry <$> findEntryByPath path archive
  pure DocxParts
    { dpComments           = part "word/comments.xml"
    , dpCommentsExtended   = part "word/commentsExtended.xml"
    , dpCommentsIds        = part "word/commentsIds.xml"
    , dpCommentsExtensible = part "word/commentsExtensible.xml"
    }

readArchive :: ByteString -> Either Text Archive
readArchive bytes = case toArchiveOrFail bytes of
  Left err      -> Left ("not a readable .docx: " <> T.pack err)
  Right archive -> Right archive
