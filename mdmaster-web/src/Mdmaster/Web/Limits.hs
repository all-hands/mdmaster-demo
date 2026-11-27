module Mdmaster.Web.Limits
  ( Limits (..)
  , limitsFromConfig
  , parseOptions
  , parseUpload
  , BodyLimitExceeded (..)
  , maxRequestBytes
  , formFieldBytes
  , fileFramingBytes
  , fieldFramingBytes
  , maxParts
  , maxFormFields
  , maxPathBytes
  , pathFieldName
  , parseFailureMessage
  , UploadError (..)
  , uploadErrorMessage
  , acceptsPath
  , safeRelativePath
  , acceptedPhrase
  , pairPaths
  , checkUpload
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl, toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Docx.Admission (DocxAdmissionError, DocxLimits (..), admissionMessage)
import Mdmaster.Core.Operation (InputFormat (..), inputExtensions)
import Mdmaster.Web.Config (Config (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Parse
import System.FilePath (splitDirectories, takeExtension)

data Limits = Limits
  { limMaxFiles     :: Int
  , limMaxFileBytes :: Int
  , limMaxBatchBytes :: Int
  , limDocx :: DocxLimits
  }
  deriving (Eq, Show)

limitsFromConfig :: Config -> Limits
limitsFromConfig config = Limits
  { limMaxFiles        = cfgMaxFiles config
  , limMaxFileBytes    = cfgMaxFileBytes config
  , limMaxBatchBytes   = cfgMaxBatchBytes config
  , limDocx            = DocxLimits
      { dlFileBytes  = cfgMaxDocxUncompressedBytes config
      , dlBatchBytes = cfgMaxDocxBatchUncompressedBytes config
      }
  }

parseOptions :: Limits -> Parse.ParseRequestBodyOptions
parseOptions limits =
  Parse.setMaxRequestNumFiles (limMaxFiles limits)
    . Parse.setMaxRequestFileSize (fromIntegral (limMaxFileBytes limits))
    . Parse.setMaxRequestFilesSize (fromIntegral (limMaxBatchBytes limits))
    . Parse.setMaxRequestParmsSize (formFieldBytes + pathBudget)
    $ Parse.defaultParseRequestBodyOptions
  where
    pathBudget = limMaxFiles limits * (BS.length pathFieldName + maxPathBytes)

formFieldBytes :: Int
formFieldBytes = 65336

pathFieldName :: ByteString
pathFieldName = "paths"

maxPathBytes :: Int
maxPathBytes = 1024

maxFormFields :: Int
maxFormFields = 64

maxParts :: Limits -> Int
maxParts limits = 2 * limMaxFiles limits + maxFormFields

maxRequestBytes :: Limits -> Int
maxRequestBytes limits =
  limMaxBatchBytes limits + limMaxFiles limits * fileFramingBytes + fieldFramingBytes

fileFramingBytes :: Int
fileFramingBytes = 2 * partOverheadBytes + 4 * maxPathBytes

fieldFramingBytes :: Int
fieldFramingBytes = formFieldBytes + (maxFormFields + 1) * partOverheadBytes

partOverheadBytes :: Int
partOverheadBytes = 512

data BodyLimitExceeded
  = TooManyParts Int
  deriving (Eq, Show)

instance Exception BodyLimitExceeded

parseUpload :: Limits -> Wai.Request -> IO ([Parse.Param], [Parse.File BL.ByteString])
parseUpload limits request = do
  next <- counted (Wai.getRequestBodyChunk request)
  Parse.parseRequestBodyEx (parseOptions limits) Parse.lbsBackEnd
    (Wai.setRequestBodyChunks next request)
  where
    delimiter = case Parse.getRequestBodyType request of
      Just (Parse.Multipart boundary) -> Just ("--" <> boundary)
      _                               -> Nothing

    counted source = case delimiter of
      Nothing -> pure source
      Just d  -> do
        state <- newIORef (0, BS.empty)
        pure $ do
          chunk <- source
          (parts, carry) <- readIORef state
          let window = carry <> chunk
              found  = parts + occurrences d window
          when (found > maxParts limits + 1) $
            throwIO (TooManyParts (maxParts limits))
          writeIORef state (found, BS.drop (BS.length window - (BS.length d - 1)) window)
          pure chunk

    occurrences needle = go 0
      where
        go n haystack =
          let (_, rest) = BS.breakSubstring needle haystack
           in if BS.null rest then n else go (n + 1 :: Int) (BS.drop (BS.length needle) rest)

parseFailureMessage :: Text
parseFailureMessage = "The upload exceeded the configured limits."

data UploadError
  = NoAcceptableFiles InputFormat
  | TooManyFiles Int Int
  | FileTooLarge FilePath Int Int
  | BatchTooLarge Int Int
  | UnsafePath FilePath
  | PathTooLong Int Int
  | PathCountMismatch Int Int
  | NotUtf8 FilePath
  | DocxRejected DocxAdmissionError
  deriving (Eq, Show)

uploadErrorMessage :: UploadError -> Text
uploadErrorMessage e = case e of
  NoAcceptableFiles format ->
    "No usable files were uploaded. Only " <> acceptedPhrase format
      <> " files are accepted."
  TooManyFiles actual limit ->
    tshow actual <> " files uploaded, over the limit of " <> tshow limit
  FileTooLarge path actual limit ->
    T.pack path <> " is " <> tshow actual <> " bytes, over the "
      <> tshow limit <> " byte per-file limit"
  BatchTooLarge actual limit ->
    "The upload totals " <> tshow actual <> " bytes, over the "
      <> tshow limit <> " byte limit"
  UnsafePath path ->
    "The path " <> T.pack (show path) <> " is not a safe relative path. \
    \Names may not start with a slash or contain \"..\" segments, \
    \backslashes, line breaks or other control characters; rename the file \
    \and upload it again."
  PathTooLong actual limit ->
    "A file path is " <> tshow actual <> " bytes long, over the limit of "
      <> tshow limit <> " bytes"
  PathCountMismatch paths files ->
    "The upload named " <> tshow paths <> " paths for " <> tshow files
      <> " files, so the files could not be matched to their names. \
         \Reload the page and try again."
  NotUtf8 path ->
    T.pack path <> " is not valid UTF-8 text"
  DocxRejected rejection -> admissionMessage rejection
  where
    tshow = T.pack . show

acceptedPhrase :: InputFormat -> Text
acceptedPhrase = T.intercalate " and " . map T.pack . inputExtensions

acceptsPath :: InputFormat -> FilePath -> Bool
acceptsPath format path =
  map toLower (takeExtension path) `elem` inputExtensions format

safeRelativePath :: FilePath -> Maybe FilePath
safeRelativePath path
  | null path = Nothing
  | any isControl path = Nothing
  | '\\' `elem` path = Nothing
  | "/" `isPrefixOf` path = Nothing
  | any (== "..") parts = Nothing
  | null cleaned = Nothing
  | otherwise = Just (joinWith '/' cleaned)
  where
    parts = splitDirectories path
    cleaned = filter (\p -> p /= "." && p /= "/") parts
    joinWith sep = foldr1 (\a b -> a <> [sep] <> b)

pairPaths
  :: [FilePath]
  -> [(FilePath, ByteString)]
  -> Either UploadError [(FilePath, ByteString)]
pairPaths paths uploads
  | null paths                      = Right uploads
  | length paths /= length uploads  = Left (PathCountMismatch (length paths) (length uploads))
  | otherwise                       = Right (zip paths (map snd uploads))

checkUpload
  :: Limits
  -> InputFormat
  -> [(FilePath, ByteString)]
  -> Either UploadError [(FilePath, ByteString)]
checkUpload limits format uploads = do
  safe <- fmap latestPerPath (traverse checkPath uploads)
  let accepted = filter (acceptsPath format . fst) safe
  if null accepted then Left (NoAcceptableFiles format) else Right ()
  mapM_ checkFileSize accepted
  let count = length accepted
  if count > limMaxFiles limits then Left (TooManyFiles count (limMaxFiles limits)) else Right ()
  let total = sum (map (BS.length . snd) accepted)
  if total > limMaxBatchBytes limits
    then Left (BatchTooLarge total (limMaxBatchBytes limits))
    else Right ()
  Right accepted
  where
    latestPerPath files =
      let latest = M.fromList files
       in [(path, bytes) | path <- firstSeen (map fst files), Just bytes <- [M.lookup path latest]]

    firstSeen = go S.empty
      where
        go _ [] = []
        go seen (x : xs)
          | x `S.member` seen = go seen xs
          | otherwise         = x : go (S.insert x seen) xs

    checkPath (path, bytes)
      | pathBytes > maxPathBytes = Left (PathTooLong pathBytes maxPathBytes)
      | otherwise = case safeRelativePath path of
          Nothing      -> Left (UnsafePath path)
          Just cleaned -> Right (cleaned, bytes)
      where
        pathBytes = BS.length (TE.encodeUtf8 (T.pack path))

    checkFileSize (path, bytes)
      | BS.length bytes > limMaxFileBytes limits =
          Left (FileTooLarge path (BS.length bytes) (limMaxFileBytes limits))
      | otherwise = Right ()
