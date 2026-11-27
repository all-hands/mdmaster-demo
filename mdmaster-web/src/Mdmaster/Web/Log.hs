module Mdmaster.Web.Log
  ( logEvent
  , logRun
  , batchTag
  , RejectReason (..)
  , rejectReasonText
  , logReject
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (NominalDiffTime)
import Mdmaster.App.Types (BatchId (..))
import Mdmaster.Core.Operation (OperationId (..))
import System.IO (hFlush, stdout)

batchTag :: BatchId -> Text
batchTag (BatchId bid) =
  T.take 8 (T.pack (show (hashWith SHA256 (TE.encodeUtf8 bid))))

logEvent :: Text -> [(Text, Text)] -> IO ()
logEvent event fields = do
  TIO.putStrLn (T.intercalate " " (("event=" <> event) : map render fields))
  hFlush stdout
  where
    render (k, v) = k <> "=" <> v

data RejectReason
  = ReasonBusy
  | ReasonLimits
  | ReasonOptions
  | ReasonOperation
  | ReasonUpload
  | ReasonRunFailed
  | ReasonRunTimeout
  deriving (Eq, Show, Enum, Bounded)

rejectReasonText :: RejectReason -> Text
rejectReasonText reason = case reason of
  ReasonBusy       -> "busy"
  ReasonLimits     -> "limits"
  ReasonOptions    -> "options"
  ReasonOperation  -> "operation"
  ReasonUpload     -> "upload"
  ReasonRunFailed  -> "run-failed"
  ReasonRunTimeout -> "run-timeout"

logReject :: RejectReason -> IO ()
logReject reason = logEvent "reject" [("reason", rejectReasonText reason)]

logRun :: Text -> BatchId -> OperationId -> Int -> NominalDiffTime -> IO ()
logRun identity batchId operationId fileCount elapsed =
  logEvent "run"
    [ ("identity",  identity)
    , ("batch",     batchTag batchId)
    , ("operation", unOperationId operationId)
    , ("files",     T.pack (show fileCount))
    , ("ms",        T.pack (show (round (realToFrac elapsed * 1000 :: Double) :: Int)))
    ]
