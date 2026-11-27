{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mdmaster.Web.Api
  ( API
  , batchZipHref
  , batchFileDiffHref
  , batchFileHref
  , runAction
  , staticHref
  , operationsHref
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid (Html)
import Mdmaster.App.Types (BatchId (..))
import Servant
import Servant.HTML.Lucid (HTML)

type Home = Get '[HTML] (Html ())

type OperationsRoute =
  "operations" :> QueryParam "operation" Text :> Get '[HTML] (Html ())

type RunRoute = "run" :> Raw

type BatchFileDiff =
  "batch" :> Capture "bid" Text :> "file" :> Capture "n" Int :> "diff"
    :> QueryParam "context" Text :> Get '[HTML] (Html ())

type BatchFileDownload =
  "batch" :> Capture "bid" Text :> "file" :> Capture "n" Int :> "download"
    :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString)

type BatchZipDownload =
  "batch" :> Capture "bid" Text :> "download"
    :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString)

type HealthRoute = "healthz" :> Get '[PlainText] Text

type StaticRoute = "static" :> Raw

type API =
       Home
  :<|> OperationsRoute
  :<|> RunRoute
  :<|> BatchFileDiff
  :<|> BatchFileDownload
  :<|> BatchZipDownload
  :<|> HealthRoute
  :<|> StaticRoute

api :: Proxy API
api = Proxy

href :: Link -> Text
href l = "/" <> T.pack (show (linkURI l))

batchZipHref :: BatchId -> Text
batchZipHref bid = href (safeLink api (Proxy :: Proxy BatchZipDownload) (unBatchId bid))

batchFileDiffHref :: BatchId -> Int -> Maybe Int -> Text
batchFileDiffHref bid n context =
  href (safeLink api (Proxy :: Proxy BatchFileDiff) (unBatchId bid) n (tshow <$> context))
  where
    tshow = T.pack . show

batchFileHref :: BatchId -> Int -> Text
batchFileHref bid n = href (safeLink api (Proxy :: Proxy BatchFileDownload) (unBatchId bid) n)

runAction :: Text
runAction = href (safeLink api (Proxy :: Proxy RunRoute))

staticHref :: Text -> Text
staticHref name = href (safeLink api (Proxy :: Proxy StaticRoute)) <> "/" <> name

operationsHref :: Text
operationsHref = href (safeLink api (Proxy :: Proxy OperationsRoute) Nothing)
