{-# LANGUAGE ScopedTypeVariables #-}

module Mdmaster.Web.TestSupport
  ( docxFixture
  , craftedDocx
  , corruptDocx
  , notWordDocx
  , capped
  , captureStdout
  , spin
  , spinSteps
  , onOwnThread
  , Part (..)
  , multipartContentType
  , multipartBody
  , fileBody
  , paddedFileBody
  , heaviestContentType
  , heaviestBody
  , parserShortfall
  , md
  , firstOf
  , frPath
  , frStatus
  , frResult
  , frMedia
  ) where

import Codec.Archive.Zip
  (Entry (..), addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, throwIO, try)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.App.Types (BatchFile (..), FileStatus (..), rowPath)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Limits
  (Limits (..), formFieldBytes, maxFormFields, maxPathBytes)
import System.Timeout (timeout)
import System.Environment (lookupEnv)
import System.IO (hClose, hFlush, stdout)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Stack (HasCallStack)
import System.Posix.Files (removeLink)
import System.Posix.Temp (mkstemp)

docxFixture :: FilePath
docxFixture = "test/fixtures/comments.docx"

craftedDocx :: Int -> BS.ByteString
craftedDocx mb = BL.toStrict (fromArchive (addEntryToArchive lying emptyArchive))
  where
    zeros  = BL.replicate (fromIntegral mb * 1024 * 1024) 0
    lying  = (toEntry "word/document.xml" 0 zeros) { eUncompressedSize = 1 }

corruptDocx :: BS.ByteString
corruptDocx =
  BL.toStrict
    (fromArchive (addEntryToArchive cut (addEntryToArchive document emptyArchive)))
  where
    document = toEntry "word/document.xml" 0 "<w:document/>"
    whole    = toEntry "word/media/image1.png" 0 (BL.replicate (4 * 1024 * 1024) 0)
    kept     = BL.take (BL.length (eCompressedData whole) - 64) (eCompressedData whole)
    cut      = whole
      { eCompressedData = kept
      , eCompressedSize = fromIntegral (BL.length kept)
      }

notWordDocx :: BS.ByteString
notWordDocx =
  BL.toStrict
    (fromArchive (addEntryToArchive (toEntry "word/media/image1.png" 0 "png") emptyArchive))

capped :: Int -> Int -> Config
capped fileMb batchMb = defaultConfig
  { cfgMaxDocxUncompressedBytes      = fileMb * 1024 * 1024
  , cfgMaxDocxBatchUncompressedBytes = batchMb * 1024 * 1024
  }

captureStdout :: IO a -> IO (a, BS.ByteString)
captureStdout action = do
  dir <- maybe "/tmp" id <$> lookupEnv "TMPDIR"
  (path, scratch) <- mkstemp (dir <> "/mdmaster-stdout-XXXXXX")
  hFlush stdout
  saved <- hDuplicate stdout
  hDuplicateTo scratch stdout
  result <- action `finally` (hFlush stdout >> hDuplicateTo saved stdout >> hClose saved)
  hClose scratch
  written <- BS.readFile path
  removeLink path
  pure (result, written)

spin :: Int -> Int
spin steps = M.size (foldl' step M.empty [1 .. steps])
  where
    step m k = M.insert (k `mod` 4096) k m

spinSteps :: Int
spinSteps = 40 * 1000 * 1000

onOwnThread :: Int -> IO a -> IO (Maybe a)
onOwnThread micros action = do
  done <- newEmptyMVar
  _ <- forkIO (try action >>= putMVar done)
  waited <- timeout micros (takeMVar done)
  case waited of
    Nothing -> pure Nothing
    Just (Left (e :: SomeException)) -> throwIO e
    Just (Right a) -> pure (Just a)

data Part
  = Field BS.ByteString BS.ByteString
  | Upload BS.ByteString BS.ByteString
  | PaddedUpload Int BS.ByteString BS.ByteString

padLineBytes :: Int
padLineBytes = 8000

boundary :: BS.ByteString
boundary = "mdmasterTestBoundary"

multipartContentType :: BS.ByteString
multipartContentType = "multipart/form-data; boundary=" <> boundary

multipartBody :: [Part] -> BL.ByteString
multipartBody parts =
    BL.fromChunks (concatMap render parts <> ["--", boundary, "--", crlf])
  where
    crlf = "\r\n"

    render (Field name value) =
      [ "--", boundary, crlf
      , "Content-Disposition: form-data; name=\"", name, "\"", crlf
      , crlf, value, crlf
      ]
    render (Upload filename content) =
      [ "--", boundary, crlf
      , "Content-Disposition: form-data; name=\"files\"; filename=\""
      , filename, "\"", crlf
      , "Content-Type: application/octet-stream", crlf
      , crlf, content, crlf
      ]
    render (PaddedUpload padLines filename content) =
      [ "--", boundary, crlf
      , "Content-Disposition: form-data; name=\"files\"; filename=\""
      , filename, "\"", crlf
      , "Content-Type: application/octet-stream", crlf
      ]
      <> concat (replicate padLines ["X-Pad: ", padding, crlf])
      <> [crlf, content, crlf]

    padding = BS8.replicate (padLineBytes - 7) 'a'

fileBody :: [(FilePath, BS.ByteString)] -> BL.ByteString
fileBody files = multipartBody [Upload (BS8.pack path) content | (path, content) <- files]

md :: FilePath -> BS.ByteString -> (FilePath, BS.ByteString)
md path bytes = (path, bytes)

paddedFileBody :: Int -> [FilePath] -> BL.ByteString
paddedFileBody padLines files =
  BL.fromChunks (concatMap render files <> ["--", boundary, "--", crlf])
  where
    crlf = "\r\n"
    pad = BS8.replicate (8190 - 7) 'a'
    render path =
      [ "--", boundary, crlf
      , "Content-Disposition: form-data; name=\"files\"; filename=\""
      , BS8.pack path, "\"", crlf
      , "Content-Type: application/octet-stream", crlf ]
      <> concat [ ["X-Pad: ", pad, crlf] | _ <- [1 .. padLines] ]
      <> [ crlf, crlf ]

parserShortfall :: Int
parserShortfall = 4096

heaviestContentType :: BS.ByteString
heaviestContentType = "multipart/form-data; boundary=" <> heaviestBoundary

heaviestBoundary :: BS.ByteString
heaviestBoundary = BS8.pack (take 70 ("mdmasterHeaviest" <> repeat 'x'))

heaviestBody :: Limits -> BL.ByteString
heaviestBody limits =
  BL.fromChunks (concatMap field fields <> concat (zipWith file [1 ..] sizes) <> closing)
  where
    crlf = "\r\n"
    delimiter = ["--", heaviestBoundary, crlf]
    closing = ["--", heaviestBoundary, "--", crlf]

    n = limMaxFiles limits
    sizes = replicate n (limMaxBatchBytes limits `div` n - parserShortfall)

    field (name, value) =
      delimiter <> ["Content-Disposition: form-data; name=\"", name, "\"", crlf, crlf, value, crlf]

    fields =
      let operation = ("operation", "toc")
          names = [ BS8.pack (take 32 ("option" <> show i <> repeat 'o')) | i <- [1 .. maxFormFields - 1] ]
          left = formFieldBytes - 12 - sum (map BS.length names)
          (each, extra) = left `divMod` length names
          values = (each + extra) : replicate (length names - 1) each
       in operation : zip names [ BS8.replicate v 'v' | v <- values ]

    file :: Int -> Int -> [BS.ByteString]
    file i size =
      let suffix = show i <> ".md"
          path = BS8.pack (replicate (maxPathBytes - length suffix) '"' <> suffix)
          encoded = BS8.concatMap (\c -> if c == '"' then "%22" else BS8.singleton c) path
       in delimiter
            <> [ "Content-Disposition: form-data; name=\"files\"; filename=\"", encoded, "\"", crlf
               , "Content-Type: application/vnd.openxmlformats-officedocument.wordprocessingml.document", crlf
               , crlf, BS8.replicate size 'a', crlf ]
            <> field ("paths", path)

firstOf :: HasCallStack => String -> [a] -> a
firstOf what xs = case xs of
  x : _ -> x
  []    -> error ("expected " <> what <> ", found none")

frPath :: BatchFile -> FilePath
frPath = rowPath

frStatus :: BatchFile -> FileStatus
frStatus row = case row of
  DocRow _ status _ _ _ -> status
  MediaRow {}           -> Unchanged

frResult :: BatchFile -> Maybe Text
frResult row = case row of
  DocRow _ _ result _ _ -> result
  MediaRow {}           -> Nothing

frMedia :: BatchFile -> Maybe BS.ByteString
frMedia row = case row of
  MediaRow _ bytes -> Just bytes
  DocRow {}        -> Nothing
