{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mdmaster.Web.Handlers
  ( AppEnv (..)
  , indexHandler
  , optionsHandler
  , diffHandler
  , fileDownloadHandler
  , zipDownloadHandler
  , healthHandler
  , runApplication
  , contentDisposition
  , healthzBody
  , newBatchId
  , unknownOperationMessage
  , RunFailure (..)
  , discardBody
  , forceBatch
  , runFailedMessage
  , runTimeoutMessage
  , runBusyMessage
  , runBusyRetrySeconds
  ) where

import Control.DeepSeq (force)
import Control.Exception
  ( AsyncException (HeapOverflow, StackOverflow)
  , SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , throwIO
  , try
  )
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isControl)
import qualified Data.Map.Strict as M
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import Lucid (Html, renderBS)
import Mdmaster.App.Diff (diffHunks)
import Mdmaster.App.FileStore (FileStore (..))
import Mdmaster.App.Pipeline (diffContext, runBatch)
import Mdmaster.App.Types
import Mdmaster.App.Zip (zipBatchChecked, zipEntryPaths)
import Mdmaster.Core.Operation
import Mdmaster.Core.Options (validateOptions)
import Mdmaster.Web.Config (Config (..))
import Mdmaster.Web.Identity (IdentityProvider (..))
import Mdmaster.Web.Limits
  ( limitsFromConfig
  , pairPaths
  , parseFailureMessage
  , parseUpload
  , pathFieldName
  , uploadErrorMessage
  )
import Mdmaster.Web.Log
  (RejectReason (..), logReject, logRun)
import Mdmaster.Web.RunGate (RunGate, withRunPermit)
import Mdmaster.Web.Upload (extractDocumentsIO)
import Mdmaster.Web.View.Demo (samplePanel)
import Mdmaster.Web.View.Diff (diffView)
import Mdmaster.Web.View.DiffContext
  (badDiffContextMessage, clampDiffContext, parseDiffContext)
import Mdmaster.Web.View.Form (operationForm, optionPanel)
import Mdmaster.Web.View.Layout (dropOverlay, dropZone, formPanel, page)
import Mdmaster.Web.View.Results
  ( emptyResults
  , errorBanner
  , errorBannerTemplate
  , expiredView
  , fileListView
  , nothingToZipView
  )
import Network.HTTP.Types (status200, status400, status429, urlEncode)
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Parse
import Numeric (showHex)
import System.Timeout (timeout)
import Servant hiding (respond)

data AppEnv = AppEnv
  { envConfig     :: Config
  , envStore      :: FileStore IO
  , envOperations :: [Operation]
  , envIdentity   :: IdentityProvider
  , envRunGate    :: RunGate
  }

indexHandler :: AppEnv -> Handler (Html ())
indexHandler env = pure $ page (envConfig env) "mdmaster" $ do
  mapM_ (samplePanel (envOperations env)) (cfgDemo (envConfig env))
  dropZone (limitsFromConfig (envConfig env)) (defaultInput env)
  formPanel $
    operationForm (limitsFromConfig (envConfig env)) (envOperations env)
      (cfgDefaultOperation (envConfig env)) M.empty []
  emptyResults
  errorBannerTemplate
  dropOverlay

defaultInput :: AppEnv -> InputFormat
defaultInput env =
  maybe MarkdownInput opInput
    (lookupOperation (cfgDefaultOperation (envConfig env)) (envOperations env))

optionsHandler :: AppEnv -> Maybe Text -> Handler (Html ())
optionsHandler env requested =
  case lookupOperation (OperationId (fromMaybe "" requested)) (envOperations env) of
    Nothing        -> pure (errorBanner unknownOperationMessage)
    Just operation -> pure (optionPanel operation M.empty [])

diffHandler :: AppEnv -> Text -> Int -> Maybe Text -> Handler (Html ())
diffHandler env batchIdText index rawContext = do
  context <- case rawContext of
    Nothing    -> pure diffContext
    Just value -> case parseDiffContext value of
      Nothing -> throwHtml (envConfig env) err400 "Bad request" (errorBanner badDiffContextMessage)
      Just n  -> pure (clampDiffContext n)
  found <- liftIO (fsGet (envStore env) (BatchId batchIdText))
  case found >>= fileAt index of
    Nothing   -> throwGone (envConfig env)
    Just file -> pure $ diffView (BatchId batchIdText) index context
                   (rediffed context file)
  where
    rediffed ctx file = case file of
      DocRow path status result before hunks -> DocRow path status result before $
        case (before, result) of
          (Just old, Just new) -> Just (diffHunks ctx old new)
          _                    -> hunks
      MediaRow {} -> file

fileDownloadHandler
  :: AppEnv -> Text -> Int
  -> Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
fileDownloadHandler env batchIdText index = do
  found <- liftIO (fsGet (envStore env) (BatchId batchIdText))
  case found >>= fileAt index of
    Nothing -> throwGone (envConfig env)
    Just file -> case fileBytes file of
      Nothing -> throwGone (envConfig env)
      Just bytes -> pure $
        addHeader (contentDisposition (T.pack (rowPath file)))
                  (BL.fromStrict bytes)

zipDownloadHandler
  :: AppEnv -> Text
  -> Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
zipDownloadHandler env batchIdText = do
  found <- liftIO (fsGet (envStore env) (BatchId batchIdText))
  case found of
    Nothing -> throwGone (envConfig env)
    Just batch
      | null (zipEntryPaths batch) -> throwNothingToZip (envConfig env)
      | otherwise -> case zipBatchChecked batch of
          Right archive -> pure (addHeader (contentDisposition "mdmaster.zip") archive)
          Left _ -> throwHtml (envConfig env) err500 "Download failed"
                      (errorBanner zipRefusedMessage)

healthHandler :: Handler Text
healthHandler = pure healthzBody

throwGone :: Config -> Handler a
throwGone config = throwHtml config err410 "Expired" expiredView

zipRefusedMessage :: Text
zipRefusedMessage =
  "This batch could not be packed into a zip. Download its files one at a time instead."

throwNothingToZip :: Config -> Handler a
throwNothingToZip config = throwHtml config err409 "Nothing to download" nothingToZipView

throwHtml :: Config -> ServerError -> Text -> Html () -> Handler a
throwHtml config err title body = throwError err
  { errBody    = renderBS (page config title body)
  , errHeaders = [("Content-Type", "text/html; charset=utf-8")]
  }

fileAt :: Int -> BatchResult -> Maybe BatchFile
fileAt index batch
  | index < 0 = Nothing
  | otherwise = case drop index (brFiles batch) of
      (file : _) -> Just file
      []         -> Nothing

contentDisposition :: Text -> Text
contentDisposition name
  | T.all isAscii cleaned = quoted
  | otherwise             = quoted <> "; filename*=UTF-8''" <> percentEncoded
  where
    cleaned = T.filter (\c -> c /= '"' && not (isControl c)) name
    quoted = "attachment; filename=\"" <> T.map asciiOnly cleaned <> "\""
    asciiOnly c = if isAscii c then c else '_'
    percentEncoded = TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 cleaned))

runApplication :: AppEnv -> Wai.Application
runApplication env request respond =
  respond =<< withRunPermit (envRunGate env) busy (do
    started <- getCurrentTime
    identity <- resolveIdentity (envIdentity env) (Wai.requestHeaders request)
    parsed <- try (parseUpload limits request)
    case parsed of
      Left (e :: SomeException)
        | isAsynchronous e -> throwIO e
        | otherwise        -> badRequest parseFailureMessage
      Right (params, files) -> handleParsed identity started params files)
  where
    config = envConfig env

    limits = limitsFromConfig config

    handleParsed identity started params files = do
      let (pathFields, formFields) = partition ((== pathFieldName) . fst) params
          paths = map (decodeString . snd) pathFields
          submitted = M.fromList
            [ (decode k, decode v) | (k, v) <- formFields ]
          requested = OperationId (M.findWithDefault "" "operation" submitted)
      case lookupOperation requested (envOperations env) of
        Nothing ->
          rejectWith ReasonOperation unknownOperationMessage submitted
        Just operation ->
          case validateOptions (opSchema operation) submitted of
            Left errors -> formErrorFor operation submitted errors
            Right values -> do
              extracted <- case pairPaths paths (map toUpload files) of
                Left mismatch -> pure (Left mismatch)
                Right uploads -> extractDocumentsIO config (opInput operation) uploads
              case extracted of
                Left uploadError ->
                  rejectWith ReasonUpload (uploadErrorMessage uploadError) submitted
                Right documents -> do
                  batchId <- newBatchId
                  forced <- forceBatch (cfgRunTimeout config)
                    (runBatch started batchId operation values documents)
                  case forced of
                    Left RunFailed ->
                      rejectRun ReasonRunFailed runFailedMessage operation submitted
                    Left RunTimedOut ->
                      rejectRun ReasonRunTimeout
                        (runTimeoutMessage (cfgRunTimeout config)) operation submitted
                    Right batch -> storeAndRespond identity started operation documents batch

    storeAndRespond identity started operation documents batch = do
      expiresAt <- fsPut (envStore env) (brId batch) batch
      finished <- getCurrentTime
      logRun identity (brId batch) (opId operation)
             (length documents) (diffUTCTime finished started)
      respondHtml status200 (fileListView finished expiresAt batch)

    toUpload (_, fileInfo) =
      ( decodeString (Parse.fileName fileInfo)
      , BL.toStrict (Parse.fileContent fileInfo)
      )

    decode = TE.decodeUtf8With (\_ _ -> Just '\xfffd')
    decodeString = T.unpack . decode

    reject reason banner selected submitted errors = do
      logReject reason
      respondHtml status400 $ formPanel $ do
        mapM_ errorBanner banner
        operationForm (limitsFromConfig (envConfig env)) (envOperations env) selected submitted errors

    formErrorFor operation submitted errors =
      reject ReasonOptions Nothing (opId operation) submitted errors

    rejectWith reason message submitted =
      reject reason (Just message) (cfgDefaultOperation config) submitted []

    rejectRun reason message operation submitted =
      reject reason (Just message) (opId operation) submitted []

    badRequest message =
      reject ReasonLimits (Just message) (cfgDefaultOperation config) M.empty []

    respondHtml status html = do
      let bytes = renderBS html
      _ <- evaluate (BL.length bytes)
      pure $ Wai.responseLBS status
        [("Content-Type", "text/html; charset=utf-8")]
        bytes

    busy = do
      logReject ReasonBusy
      pure $ Wai.responseLBS status429
        [ ("Content-Type", "text/plain; charset=utf-8")
        , ("Retry-After", TE.encodeUtf8 (T.pack (show runBusyRetrySeconds)))
        ]
        (BL.fromStrict (TE.encodeUtf8 runBusyMessage))

discardBody :: Int -> IO BS.ByteString -> IO ()
discardBody bound nextChunk = go 0
  where
    go seen
      | seen >= bound = pure ()
      | otherwise = do
          chunk <- nextChunk
          if BS.null chunk then pure () else go (seen + BS.length chunk)

unknownOperationMessage :: Text
unknownOperationMessage = "Unknown operation."

data RunFailure
  = RunFailed
  | RunTimedOut
  deriving (Eq, Show)

forceBatch :: NominalDiffTime -> BatchResult -> IO (Either RunFailure BatchResult)
forceBatch limit batch = do
  outcome <- try (timeout micros (evaluate (force batch)))
  case outcome of
    Right (Just forced) -> pure (Right forced)
    Right Nothing       -> pure (Left RunTimedOut)
    Left (e :: SomeException)
      | isCancellation e -> throwIO e
      | otherwise        -> pure (Left RunFailed)
  where
    micros = ceiling (limit * 1000000)

    isCancellation e = case fromException e of
      Just StackOverflow -> False
      Just HeapOverflow  -> False
      _                  -> isAsynchronous e

isAsynchronous :: SomeException -> Bool
isAsynchronous e = case fromException e of
  Just (_ :: SomeAsyncException) -> True
  Nothing                        -> False

runFailedMessage :: Text
runFailedMessage =
  "This run failed while processing your files, and nothing was kept. \
  \Try again, or remove the file that triggers it."

runTimeoutMessage :: NominalDiffTime -> Text
runTimeoutMessage limit =
  "This run took longer than " <> T.pack (show (round limit :: Int))
    <> " seconds and was stopped, and nothing was kept. \
       \Try fewer or smaller files."

runBusyMessage :: Text
runBusyMessage =
  "The server is already running as many batches as it can hold. \
  \Wait a moment and try again."

runBusyRetrySeconds :: Int
runBusyRetrySeconds = 30

healthzBody :: Text
healthzBody = "ok"

newBatchId :: IO BatchId
newBatchId = do
  bytes <- getRandomBytes 16
  pure (BatchId (T.concat (map hexByte (BS.unpack bytes))))
  where
    hexByte b = T.justifyRight 2 '0' (T.pack (showHex b ""))
