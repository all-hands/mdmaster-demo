{-# LANGUAGE ScopedTypeVariables #-}

module Mdmaster.App.Docx.Admission
  ( DocxLimits (..)
  , DocxAdmissionError (..)
  , admissionMessage
  , admitDocx
  , admitDocxIO
  ) where

import Codec.Archive.Zip (Archive, Entry (..), findEntryByPath, fromEntry, zEntries)
import Codec.Compression.Zlib (DecompressError)
import Control.Exception (ErrorCall (..), Handler (..), catches, evaluate)
import Control.Monad (join)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.App.Docx.Archive (readArchive)

data DocxLimits = DocxLimits
  { dlFileBytes  :: Int
  , dlBatchBytes :: Int
  }
  deriving (Eq, Show)

data DocxAdmissionError
  = NotReadableDocx FilePath Text
  | DeclaresTooManyBytes FilePath Int Int
  | NotWordDocx FilePath
  | ExpandsTooFar FilePath Text
  | BatchExpandsTooFar Int
  | CorruptDocxStream Text
  deriving (Eq, Show)

admissionMessage :: DocxAdmissionError -> Text
admissionMessage e = case e of
  NotReadableDocx path reason ->
    T.pack path <> ": " <> reason
  DeclaresTooManyBytes path declared fileCap ->
    T.pack path <> " declares " <> shown declared <> " bytes, over the "
      <> shown fileCap <> " byte limit"
  NotWordDocx path ->
    T.pack path <> ": not a Word document: word/document.xml is missing"
  ExpandsTooFar path reason ->
    T.pack path <> ": " <> reason
  BatchExpandsTooFar batchCap ->
    "The .docx files in this upload expand" <> pastLimit batchCap
  CorruptDocxStream diagnosis ->
    "A .docx in this upload could not be decompressed: " <> diagnosis
  where
    shown = T.pack . show

pastLimit :: Int -> Text
pastLimit limit =
  " past the " <> T.pack (show limit) <> " byte limit when decompressed"

admitDocx :: DocxLimits -> Int -> FilePath -> ByteString -> Either DocxAdmissionError Int
admitDocx limits used path bytes = do
  archive <- first (NotReadableDocx path) (readArchive bytes)
  let declared = sum (map (fromIntegral . eUncompressedSize) (zEntries archive))
  if declared > dlFileBytes limits
    then Left (DeclaresTooManyBytes path declared (dlFileBytes limits))
    else Right ()
  case findEntryByPath "word/document.xml" archive of
    Nothing -> Left (NotWordDocx path)
    Just _  -> Right ()
  first overBudget (inflateUnderBudget (min (dlFileBytes limits) left) archive)
  where
    left = dlBatchBytes limits - used

    overBudget reason
      | dlFileBytes limits <= left = ExpandsTooFar path reason
      | otherwise                  = BatchExpandsTooFar (dlBatchBytes limits)

admitDocxIO
  :: DocxLimits -> Int -> FilePath -> ByteString
  -> IO (Either DocxAdmissionError Int)
admitDocxIO limits used path bytes =
  fmap join (guardAdmission (admitDocx limits used path bytes))

inflateUnderBudget :: Int -> Archive -> Either Text Int
inflateUnderBudget limit archive = go 0 (zEntries archive)
  where
    go total [] = Right total
    go total (entry : rest)
      | total' > limit = Left ("expands" <> pastLimit limit)
      | otherwise = go total' rest
      where
        budget = limit - total
        total' = total + fromIntegral
          (BL.length (BL.take (fromIntegral budget + 1) (fromEntry entry)))

guardAdmission :: a -> IO (Either DocxAdmissionError a)
guardAdmission value =
    (Right <$> evaluate value) `catches`
      [ Handler (\e ->
            pure (Left (CorruptDocxStream (diagnosis (e :: DecompressError)))))
      , Handler (\(ErrorCall _) ->
            pure (Left (CorruptDocxStream "the archive is malformed")))
      ]
  where
    diagnosis e =
      let shown = T.pack (show e)
       in fromMaybe shown (T.stripPrefix "Codec.Compression.Zlib: " shown)

