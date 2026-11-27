module Mdmaster.Web.ShutdownSpec
  ( spec
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.FileStore (FileStore (..), Held (..))
import Mdmaster.App.Types (BatchId (..), BatchResult (..))
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionValues (..))
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Handlers (AppEnv (..))
import Mdmaster.Web.Server
  (runServer, serverSettings, shutdownGraceSeconds, withServerEnv)
import Network.Socket
  (Family (AF_INET), PortNumber, SockAddr (SockAddrInet), SocketType (Stream),
   bind, close, connect, defaultProtocol, listen, socket, socketPort,
   tupleToHostAddress)
import Network.Socket.ByteString (recv, send)
import Network.Wai.Handler.Warp (getGracefulShutdownTimeout, getPort)
import System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigTERM)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "serverSettings" $ do
    it "binds the configured port" $
      getPort (serverSettings defaultConfig) `shouldBe` cfgPort defaultConfig

    it "bounds the drain to the shutdown grace" $
      getGracefulShutdownTimeout (serverSettings defaultConfig)
        `shouldBe` Just shutdownGraceSeconds

  describe "SIGTERM" $
    it "stops accepting and returns from runServer" $ do
      port <- freePort
      _ <- installHandler sigTERM (Catch (pure ())) Nothing
      done <- newEmptyMVar
      _ <- forkIO (runServer (on port) `finally` putMVar done ())
      waitFor port
      raiseSignal sigTERM
      drained <- timeout tenSeconds (takeMVar done)
      _ <- installHandler sigTERM Default Nothing
      drained `shouldBe` Just ()

  describe "withServerEnv" $
    it "starts the sweeper, which empties the store of expired results while idle" $ do
      let config = defaultConfig { cfgResultTtl = 0.05 }
      emptied <- withServerEnv 20000 config $ \env -> do
        _ <- fsPut (envStore env) (BatchId "swept") (emptyBatch (BatchId "swept"))
        held <- fsHeld (envStore env)
        heldEntries held `shouldBe` 1
        waitEmpty (envStore env) (100 :: Int)
      emptied `shouldBe` True
  where
    waitEmpty _ 0 = pure False
    waitEmpty store n = do
      held <- fsHeld store
      if heldEntries held == 0
        then pure True
        else threadDelay 20000 >> waitEmpty store (n - 1)

emptyBatch :: BatchId -> BatchResult
emptyBatch bid = BatchResult
  { brId        = bid
  , brAt        = UTCTime (fromGregorian 2026 11 11) 0
  , brOperation = OperationId "any"
  , brOptions   = OptionValues M.empty
  , brInput     = MarkdownInput
  , brFiles     = []
  }

on :: PortNumber -> Config
on port = defaultConfig { cfgPort = fromIntegral port }

tenSeconds :: Int
tenSeconds = 10 * 1000000

freePort :: IO PortNumber
freePort =
  bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
    bind s (SockAddrInet 0 0)
    listen s 1
    socketPort s

waitFor :: PortNumber -> IO ()
waitFor port = go (200 :: Int)
  where
    go 0 = expectationFailure ("nothing answered on port " <> show port)
    go n = do
      up <- answering port
      unless up $ do
        threadDelay 50000
        go (n - 1)

answering :: PortNumber -> IO Bool
answering port = do
  outcome <- try (bracket (socket AF_INET Stream defaultProtocol) close ask)
  pure $ case outcome :: Either IOException Bool of
    Right ok -> ok
    Left _   -> False
  where
    ask s = do
      connect s (SockAddrInet port localhost)
      _ <- send s probe
      BS.isInfixOf "200" <$> recv s 1024
    localhost = tupleToHostAddress (127, 0, 0, 1)
    probe     = "GET /healthz HTTP/1.0\r\n\r\n"
