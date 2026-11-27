module Mdmaster.App.Zip
  ( zipBatch
  , zipBatchChecked
  , zipEntryPaths
  , maxEntryNameBytes
  , maxEntries
  ) where

import Codec.Archive.Zip (Archive (..), Entry (..), emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Types
import Mdmaster.App.Zip.Manifest (manifestEntryPath, manifestFor)

maxEntryNameBytes :: Int
maxEntryNameBytes = 65535

maxEntries :: Int
maxEntries = 65534

zipBatch :: BatchResult -> ByteString
zipBatch = either (error . T.unpack) id . zipBatchChecked

zipBatchChecked :: BatchResult -> Either Text ByteString
zipBatchChecked batch
  | any tooLong names =
      Left "an entry name is longer than 65535 bytes, which a zip header cannot carry"
  | length names > maxEntries =
      Left "the batch has more files than a zip without ZIP64 can carry (65534)"
  | otherwise = Right (fromArchive (archiveOf (manifestEntry : downloadable batch)))
  where
    names = manifestEntryPath (zipEntryPaths batch) : zipEntryPaths batch
    tooLong path = BS.length (TE.encodeUtf8 (T.pack path)) > maxEntryNameBytes

    manifestEntry = (manifestEntryPath (zipEntryPaths batch), manifestBytes)
    manifestBytes = BL.fromStrict (TE.encodeUtf8 (manifestFor batch))

zipEntryPaths :: BatchResult -> [FilePath]
zipEntryPaths = map fst . downloadable

downloadable :: BatchResult -> [(FilePath, ByteString)]
downloadable batch =
  [ (rowPath file, BL.fromStrict bytes)
  | file <- brFiles batch
  , Just bytes <- [fileBytes file]
  ]

archiveOf :: [(FilePath, ByteString)] -> Archive
archiveOf files = emptyArchive { zEntries = go Set.empty files }
  where
    go _ [] = []
    go seen ((path, content) : rest)
      | key `Set.member` seen = go seen rest
      | otherwise = e : go (Set.insert key seen) rest
      where
        e = entry path content
        key = eRelativePath e

entry :: FilePath -> ByteString -> Entry
entry path content = toEntry path 0 content
