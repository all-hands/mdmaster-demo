module Mdmaster.Web.Upload
  ( extractDocuments
  , extractDocumentsIO
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity (runIdentity)
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Docx.Admission
  (DocxAdmissionError, DocxLimits (..), admitDocx, admitDocxIO)
import Mdmaster.App.Types (DocSource (..), Document (..))
import Mdmaster.Core.Operation (InputFormat (..))
import Mdmaster.Web.Config (Config (..))
import Mdmaster.Web.Limits (Limits (..), UploadError (..), checkUpload, limitsFromConfig)

extractDocumentsIO
  :: Config
  -> InputFormat
  -> [(FilePath, ByteString)]
  -> IO (Either UploadError [Document])
extractDocumentsIO = extractDocumentsWith admitDocxIO

extractDocuments
  :: Config -> InputFormat -> [(FilePath, ByteString)] -> Either UploadError [Document]
extractDocuments config format uploads =
  runIdentity
    (extractDocumentsWith
       (\lim used path bytes -> pure (admitDocx lim used path bytes))
       config format uploads)

extractDocumentsWith
  :: Monad m
  => (DocxLimits -> Int -> FilePath -> BL.ByteString
      -> m (Either DocxAdmissionError Int))
  -> Config
  -> InputFormat
  -> [(FilePath, ByteString)]
  -> m (Either UploadError [Document])
extractDocumentsWith admit config format uploads =
    case checkUpload uploadLimits format uploads of
      Left err       -> pure (Left err)
      Right accepted -> readSources 0 accepted
  where
    uploadLimits = limitsFromConfig config

    readSources used files = case files of
      []            -> pure (Right [])
      (file : rest) -> do
        measured <- readSource used file
        case measured of
          Left err           -> pure (Left err)
          Right (doc, used') -> fmap (fmap (doc :)) (readSources used' rest)

    readSource used (path, bytes) = case format of
      MarkdownInput -> pure $ case TE.decodeUtf8' bytes of
        Left _        -> Left (NotUtf8 path)
        Right content -> Right (Document path (MarkdownSource content), used)
      DocxInput -> do
        verdict <- admit (limDocx uploadLimits) used path (BL.fromStrict bytes)
        case verdict of
          Left rejection -> pure (Left (DocxRejected rejection))
          Right inflated -> pure (Right (Document path (DocxSource bytes), used + inflated))
