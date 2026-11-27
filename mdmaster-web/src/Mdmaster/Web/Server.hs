{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mdmaster.Web.Server
  ( application
  , runServer
  , withServerEnv
  , shutdownGraceSeconds
  , sweepIntervalSeconds
  , serverSettings
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Mdmaster.App.FileStore (FileStore (..), newInMemoryStore)
import Mdmaster.Core.Operation (Operation (..))
import Mdmaster.Plugins.Registry (allOperations)
import Mdmaster.Web.Api (API)
import Mdmaster.Web.Config (Config (..), checkDefaultOperation, sweepIntervalSeconds)
import Mdmaster.Web.Handlers
import Mdmaster.Web.Headers (securityHeaders)
import Mdmaster.Web.Identity (anonymousProvider)
import Mdmaster.Web.Limits (Limits, limitsFromConfig, maxRequestBytes, parseFailureMessage)
import Mdmaster.Web.Log (logEvent)
import Mdmaster.Web.RunGate (newRunGate)
import Network.HTTP.Types (status413)
import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import Network.Wai.Handler.Warp
  (Settings, defaultSettings, runSettings, setGracefulShutdownTimeout,
   setInstallShutdownHandler, setPort, setServerName)
import Network.Wai.Middleware.RequestSizeLimit
  (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware,
   setMaxLengthForRequest, setOnLengthExceeded)
import Servant
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import WaiAppStatic.Types (MaxAge (NoMaxAge))

apiProxy :: Proxy API
apiProxy = Proxy

application :: AppEnv -> Application
application env =
  securityHeaders . drainUnread limits . bodySizeLimit limits
    $ serve apiProxy (routes env)
  where
    limits = limitsFromConfig (envConfig env)

bodySizeLimit :: Limits -> Middleware
bodySizeLimit limits = requestSizeLimitMiddleware
  . setOnLengthExceeded (\_ _ _ sendResponse -> sendResponse tooLarge)
  . setMaxLengthForRequest (const (pure (Just (fromIntegral (maxRequestBytes limits)))))
  $ defaultRequestSizeLimitSettings
  where
    tooLarge = Wai.responseLBS status413
      [("Content-Type", "text/plain; charset=utf-8")]
      (BL.fromStrict (TE.encodeUtf8 parseFailureMessage))

drainUnread :: Limits -> Middleware
drainUnread limits app request sendResponse =
  app request $ \response -> do
    discardBody (2 * maxRequestBytes limits) (Wai.getRequestBodyChunk request)
    sendResponse response

routes :: AppEnv -> Server API
routes env =
       indexHandler env
  :<|> optionsHandler env
  :<|> Tagged (runApplication env)
  :<|> diffHandler env
  :<|> fileDownloadHandler env
  :<|> zipDownloadHandler env
  :<|> healthHandler
  :<|> serveDirectoryWith
         (defaultWebAppSettings (cfgStaticDir (envConfig env)))
           { ssMaxAge = NoMaxAge }

shutdownGraceSeconds :: Int
shutdownGraceSeconds = 30

serverSettings :: Config -> Settings
serverSettings config = setPort (cfgPort config)
                      $ setServerName "mdmaster"
                      $ setInstallShutdownHandler install
                      $ setGracefulShutdownTimeout (Just shutdownGraceSeconds)
                        defaultSettings
  where
    install closeSocket =
      void (installHandler sigTERM (Catch closeSocket) Nothing)

runServer :: Config -> IO ()
runServer config = do
  case checkDefaultOperation (map opId allOperations) config of
    Left message -> fail (T.unpack message)
    Right _      -> pure ()
  withServerEnv (sweepIntervalSeconds * 1000000) config $ \env -> do
    logEvent "start"
      [ ("port",       T.pack (show (cfgPort config)))
      , ("operations", T.pack (show (length allOperations)))
      ]
    runSettings (serverSettings config) (application env)
    logEvent "stop"
      [ ("port", T.pack (show (cfgPort config)))
      ]

withServerEnv :: Int -> Config -> (AppEnv -> IO a) -> IO a
withServerEnv sweepMicros config action = do
  store <- newInMemoryStore (cfgResultTtl config) (cfgMaxResults config) (cfgMaxStoreBytes config)
  gate <- newRunGate (cfgMaxConcurrentRuns config)
  let env = AppEnv
        { envConfig     = config
        , envStore      = store
        , envOperations = allOperations
        , envIdentity   = anonymousProvider
        , envRunGate    = gate
        }
      sweeper = forever $ do
        threadDelay sweepMicros
        fsSweep store
  bracket (forkIO sweeper) killThread (const (action env))
