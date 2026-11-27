{-# LANGUAGE DeriveGeneric #-}

module Mdmaster.App.Types
  ( BatchId (..)
  , DocSource (..)
  , Document (..)
  , FileStatus (..)
  , BatchFile (DocRow, MediaRow)
  , rowPath
  , fileBytes
  , fileHunkCounts
  , BatchResult (..)
  , batchBytes
  , batchChangedCount
  , batchUnchangedCount
  , batchFailedCount
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Unsafe (lengthWord8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Mdmaster.App.Diff (DiffHunk (..), DiffLine (..), hunkCounts)
import Mdmaster.Core.Operation (InputFormat, OperationId)
import Mdmaster.Core.Options (OptionValues)

newtype BatchId = BatchId { unBatchId :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData BatchId

data DocSource
  = MarkdownSource Text
  | DocxSource ByteString
  deriving (Eq, Show)

data Document = Document
  { docPath   :: FilePath
  , docSource :: DocSource
  }
  deriving (Eq, Show)

data FileStatus
  = Changed
  | Unchanged
  | Failed Text
  deriving (Eq, Show, Generic)

instance NFData FileStatus

data BatchFile
  = DocRow
      FilePath
      FileStatus
      (Maybe Text)
      (Maybe Text)
      (Maybe [DiffHunk])
  | MediaRow
      FilePath
      ByteString
  deriving (Eq, Show, Generic)

instance NFData BatchFile

rowPath :: BatchFile -> FilePath
rowPath row = case row of
  DocRow path _ _ _ _ -> path
  MediaRow path _     -> path

fileBytes :: BatchFile -> Maybe ByteString
fileBytes row = case row of
  MediaRow _ bytes         -> Just bytes
  DocRow _ _ result _ _    -> fmap TE.encodeUtf8 result

fileHunkCounts :: BatchFile -> Maybe (Int, Int)
fileHunkCounts row = case row of
  DocRow _ _ _ _ (Just hunks@(_ : _)) -> Just (hunkCounts hunks)
  DocRow {}                           -> Nothing
  MediaRow {}                         -> Nothing

data BatchResult = BatchResult
  { brId        :: BatchId
  , brAt        :: UTCTime
  , brOperation :: OperationId
  , brOptions   :: OptionValues
  , brInput     :: InputFormat
  , brFiles     :: [BatchFile]
  }
  deriving (Eq, Show, Generic)

instance NFData BatchResult

batchBytes :: BatchResult -> Int
batchBytes = sum . map rowBytes . brFiles

rowBytes :: BatchFile -> Int
rowBytes row = case row of
  DocRow path status result before hunks -> sum
    [ length path
    , statusBytes status
    , maybe 0 textBytes result
    , maybe 0 textBytes before
    , maybe 0 (sum . map hunkBytes) hunks
    ]
  MediaRow path bytes -> length path + B.length bytes
  where
    statusBytes status = case status of
      Failed message -> textBytes message
      _              -> 0
    hunkBytes = sum . map (textBytes . dlText) . dhLines

textBytes :: Text -> Int
textBytes = lengthWord8

batchChangedCount :: BatchResult -> Int
batchChangedCount = countRows isChanged
  where
    isChanged row = case row of
      DocRow _ Changed _ _ _ -> True
      _                      -> False

batchUnchangedCount :: BatchResult -> Int
batchUnchangedCount = countRows countsAsUnchanged
  where
    countsAsUnchanged row = case row of
      DocRow _ Unchanged _ _ _ -> True
      MediaRow {}              -> True
      _                        -> False

batchFailedCount :: BatchResult -> Int
batchFailedCount = countRows isFailed
  where
    isFailed row = case row of
      DocRow _ (Failed _) _ _ _ -> True
      _                         -> False

countRows :: (BatchFile -> Bool) -> BatchResult -> Int
countRows p = length . filter p . brFiles
