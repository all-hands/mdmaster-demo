module Mdmaster.Web.Config
  ( Config (..)
  , defaultConfig
  , configFromEnv
  , maxRunTimeoutSeconds
  , maxConcurrentRuns
  , sweepIntervalSeconds
  , checkDefaultOperation
  , loadConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Mdmaster.Core.Operation (OperationId (..))
import Mdmaster.Web.Demo (DemoSet, demoSourceUrl, demoSwitch, loadDemo)
import Network.URI (URIAuth (..), parseAbsoluteURI, uriAuthority, uriScheme)
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

data Config = Config
  { cfgPort             :: Int
  , cfgMaxFileBytes     :: Int
  , cfgMaxFiles         :: Int
  , cfgMaxBatchBytes    :: Int
  , cfgMaxDocxUncompressedBytes :: Int
  , cfgMaxDocxBatchUncompressedBytes :: Int
  , cfgResultTtl        :: NominalDiffTime
  , cfgMaxResults       :: Int
  , cfgMaxStoreBytes    :: Int
  , cfgRunTimeout       :: NominalDiffTime
  , cfgMaxConcurrentRuns :: Int
  , cfgStaticDir        :: FilePath
  , cfgDefaultOperation :: OperationId
  , cfgSourceUrl        :: Text
  , cfgDemo             :: Maybe [DemoSet]
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgPort             = 8965
  , cfgMaxFileBytes     = 10 * 1024 * 1024
  , cfgMaxFiles         = 500
  , cfgMaxBatchBytes    = 100 * 1024 * 1024
  , cfgMaxDocxUncompressedBytes = 50 * 1024 * 1024
  , cfgMaxDocxBatchUncompressedBytes = 200 * 1024 * 1024
  , cfgResultTtl        = 30 * 60
  , cfgMaxResults       = 100
  , cfgMaxStoreBytes    = 1024 * 1024 * 1024
  , cfgRunTimeout       = 60
  , cfgMaxConcurrentRuns = 2
  , cfgStaticDir        = "static"
  , cfgDefaultOperation = OperationId "toc"
  , cfgSourceUrl        = "https://github.com/all-hands/mdmaster-demo"
  , cfgDemo             = Nothing
  }

configFromEnv :: [(String, String)] -> Either Text Config
configFromEnv env = do
  port      <- bounded "MDMASTER_PORT" 65535 (cfgPort defaultConfig)
  fileBytes <- positive "MDMASTER_MAX_FILE_BYTES" (cfgMaxFileBytes defaultConfig)
  files     <- positive "MDMASTER_MAX_FILES" (cfgMaxFiles defaultConfig)
  batch     <- positive "MDMASTER_MAX_BATCH_BYTES" (cfgMaxBatchBytes defaultConfig)
  docxBytes <- positive "MDMASTER_MAX_DOCX_UNCOMPRESSED_BYTES"
                 (cfgMaxDocxUncompressedBytes defaultConfig)
  docxBatch <- positive "MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES"
                 (cfgMaxDocxBatchUncompressedBytes defaultConfig)
  ttl       <- positive "MDMASTER_RESULT_TTL_SECONDS" (round (cfgResultTtl defaultConfig))
  results   <- positive "MDMASTER_MAX_RESULTS" (cfgMaxResults defaultConfig)
  storeBytes <- positive "MDMASTER_MAX_STORE_BYTES" (cfgMaxStoreBytes defaultConfig)
  _         <- storeHoldsTwoBatches batch docxBatch storeBytes
  runLimit  <- bounded "MDMASTER_RUN_TIMEOUT_SECONDS" maxRunTimeoutSeconds
                 (round (cfgRunTimeout defaultConfig))
  concurrent <- bounded "MDMASTER_MAX_CONCURRENT_RUNS" maxConcurrentRuns
                  (cfgMaxConcurrentRuns defaultConfig)
  static    <- nonEmpty "MDMASTER_STATIC_DIR" (cfgStaticDir defaultConfig)
  operation <- nonEmpty "MDMASTER_DEFAULT_OPERATION"
                 (T.unpack (unOperationId (cfgDefaultOperation defaultConfig)))
  source    <- webUrl "MDMASTER_SOURCE_URL" (T.unpack (cfgSourceUrl defaultConfig))
  pure Config
    { cfgPort             = port
    , cfgMaxFileBytes     = fileBytes
    , cfgMaxFiles         = files
    , cfgMaxBatchBytes    = batch
    , cfgMaxDocxUncompressedBytes = docxBytes
    , cfgMaxDocxBatchUncompressedBytes = docxBatch
    , cfgResultTtl        = fromIntegral (ttl :: Int)
    , cfgMaxResults       = results
    , cfgMaxStoreBytes    = storeBytes
    , cfgRunTimeout       = fromIntegral (runLimit :: Int)
    , cfgMaxConcurrentRuns = concurrent
    , cfgStaticDir        = static
    , cfgDefaultOperation = OperationId (T.pack operation)
    , cfgSourceUrl        = T.pack source
    , cfgDemo             = Nothing
    }
  where
    positive name fallback = case lookup name env of
      Nothing  -> Right fallback
      Just raw -> case readMaybe raw of
        Just n | n > 0 -> Right n
        _ -> Left (T.pack name <> " must be a positive whole number, got "
                     <> T.pack (show raw))

    bounded name limit fallback = do
      value <- positive name fallback
      if value <= limit
        then Right value
        else Left (T.pack name <> " must be between 1 and " <> T.pack (show limit)
                     <> ", got " <> T.pack (show value))

    storeHoldsTwoBatches batchCap docxBatchCap storeCap
      | storeCap `div` 8 < batchCap = Left
          ("MDMASTER_MAX_STORE_BYTES must be at least 8 times "
             <> "MDMASTER_MAX_BATCH_BYTES, got " <> T.pack (show storeCap)
             <> " for a batch cap of " <> T.pack (show batchCap)
             <> ": a Markdown batch can store about four times its upload, and the "
             <> "store must hold two of the largest batch, or one batch evicts "
             <> "everything else")
      | storeCap `div` 2 < docxBatchCap = Left
          ("MDMASTER_MAX_STORE_BYTES must be at least 2 times "
             <> "MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES, got " <> T.pack (show storeCap)
             <> " for a .docx batch cap of " <> T.pack (show docxBatchCap)
             <> ": a .docx batch can store about what it expands to, and the "
             <> "store must hold two of the largest batch, or one batch evicts "
             <> "everything else")
      | otherwise = Right ()

    nonEmpty name fallback = case lookup name env of
      Nothing -> Right fallback
      Just "" -> Left (T.pack name <> " must not be empty")
      Just v  -> Right v

    webUrl name fallback = case lookup name env of
      Nothing -> Right fallback
      Just raw -> case parseAbsoluteURI raw of
        Just uri
          | uriScheme uri `elem` ["http:", "https:"]
          , Just authority <- uriAuthority uri
          , not (null (uriRegName authority)) -> Right raw
        _ -> Left (T.pack name <> " must be an absolute http:// or https:// URL, got "
                     <> T.pack (show raw))

maxRunTimeoutSeconds :: Int
maxRunTimeoutSeconds = 24 * 60 * 60

maxConcurrentRuns :: Int
maxConcurrentRuns = 64

sweepIntervalSeconds :: Int
sweepIntervalSeconds = 60

checkDefaultOperation :: [OperationId] -> Config -> Either Text Config
checkDefaultOperation known config
  | cfgDefaultOperation config `elem` known = Right config
  | otherwise = Left $
      "MDMASTER_DEFAULT_OPERATION must name a known operation, got "
        <> T.pack (show (unOperationId (cfgDefaultOperation config)))
        <> ". Known operations: "
        <> T.intercalate ", " (map unOperationId known)

loadConfig :: IO Config
loadConfig = do
  env <- getEnvironment
  config <- orFail (configFromEnv env)
  switch <- orFail (demoSwitch env)
  demo <- loadDemo switch (cfgStaticDir config)
  pure config
    { cfgDemo = demo
    , cfgSourceUrl = case (demo, lookup "MDMASTER_SOURCE_URL" env) of
        (Just _, Nothing) -> demoSourceUrl
        _                 -> cfgSourceUrl config
    }
  where
    orFail :: Either Text a -> IO a
    orFail = either (fail . T.unpack) pure
