{-# LANGUAGE ScopedTypeVariables #-}

module Mdmaster.Web.RunLimitSpec (spec) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception
  ( AsyncException (ThreadKilled), IOException, SomeException, bracket, finally
  , fromException, try )
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), diffUTCTime, fromGregorian, getCurrentTime)
import Mdmaster.App.FileStore (newInMemoryStore)
import Mdmaster.App.Types
  (BatchId (..), BatchResult (..), BatchFile (..), FileStatus (..))
import Mdmaster.Core.Operation
  (InputFormat (..), OpError (..), Operation (..), OperationId (..))
import Mdmaster.Core.Options (OptionSchema (..), OptionValues (..))
import qualified Data.Map.Strict as M
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Handlers
  ( AppEnv (..)
  , RunFailure (..)
  , discardBody
  , forceBatch
  , runBusyMessage
  , runBusyRetrySeconds
  , runFailedMessage
  , runTimeoutMessage
  , unknownOperationMessage
  )
import Mdmaster.Web.Identity (anonymousProvider)
import Mdmaster.Web.Log (RejectReason (..), rejectReasonText)
import Mdmaster.Web.RunGate (RunGate, newRunGate, withRunPermit)
import Mdmaster.Web.Server (application)
import Mdmaster.Web.ServerSpec (contains, countOf, postRun, sampleMarkdown)
import Mdmaster.Web.TestSupport
  (Part (..), captureStdout, multipartContentType, onOwnThread, spin, spinSteps)
import Network.HTTP.Types (methodPost, status200, status400, status429)
import Network.Wai
  ( Application
  , Request (..)
  , defaultRequest
  , responseStatus
  , setRequestBodyChunks
  )
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Socket
  (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), close,
   connect, defaultProtocol, socket, tupleToHostAddress)
import Network.Socket.ByteString (recv, sendAll)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Mdmaster.Web.Limits (limitsFromConfig, maxRequestBytes, parseFailureMessage)
import Numeric (showHex)
import Text.Pandoc.Definition (Pandoc)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Wai (request, SResponse (..))
import Test.Hspec.Wai.Internal (withApplication)

crashing :: Operation
crashing = testOperation "crashes" $ \_ _ -> error crashText

crashText :: String
crashText = "the transform fell over on purpose"

runaway :: Operation
runaway = testOperation "runs-away" $ \_ doc ->
  spin (spinSteps + length (show doc)) `seq` Right doc

tenSeconds :: Int
tenSeconds = 10 * 1000 * 1000

passing :: Operation
passing = testOperation "passes" $ \_ doc -> Right doc

testOperation
  :: String
  -> (OptionValues -> Pandoc -> Either OpError Pandoc)
  -> Operation
testOperation name run = Operation
  { opId          = OperationId (textOf name)
  , opName        = textOf name
  , opDescription = textOf name
  , opInput       = MarkdownInput
  , opSchema      = OptionSchema []
  , opRun         = run
  }
  where
    textOf = T.pack

appFor :: Operation -> IO Application
appFor operation = application <$> envFor operation id

envFor :: Operation -> (Config -> Config) -> IO AppEnv
envFor operation tweak = do
  store <- newInMemoryStore (cfgResultTtl config) (cfgMaxResults config) (cfgMaxStoreBytes config)
  gate <- newRunGate (cfgMaxConcurrentRuns config)
  pure AppEnv
    { envConfig     = config
    , envStore      = store
    , envOperations = [operation]
    , envIdentity   = anonymousProvider
    , envRunGate    = gate
    }
  where
    config = tweak defaultConfig
      { cfgDefaultOperation = opId operation
      , cfgRunTimeout       = shortLimit
      }

shortLimit :: Fractional a => a
shortLimit = 0.2

runOnce :: Operation -> IO (SResponse, BS.ByteString)
runOnce operation = do
  app <- appFor operation
  (guarded, logged) <- captureStdout $ onOwnThread (5 * 1000 * 1000) $
    withApplication app $
      postRun
        [ Field "operation" (TE.encodeUtf8 (unOperationId (opId operation)))
        , Upload "notes.md" sampleMarkdown
        ]
  maybe (expectationFailure "the request did not finish in 5 seconds" >> fail "hung")
        (\res -> pure (res, logged)) guarded

logLine :: RejectReason -> BS.ByteString
logLine reason =
  "event=reject reason=" <> TE.encodeUtf8 (rejectReasonText reason)

holding :: RunGate -> IO a -> IO a
holding gate action = do
  release <- newEmptyMVar
  done <- newEmptyMVar
  released <- newEmptyMVar
  void . forkFinally
    (withRunPermit gate (pure ()) (putMVar release () >> takeMVar done)
       `finally` putMVar released ())
    $ \_ -> pure ()
  granted <- timeout (5 * 1000 * 1000) (takeMVar release)
  case granted of
    Nothing -> expectationFailure "the gate never handed out its permit"
                 >> fail "hung"
    Just () -> do
      outcome <- action `finally` putMVar done ()
      gone <- timeout (5 * 1000 * 1000) (takeMVar released)
      case gone of
        Nothing -> expectationFailure "the permit was never given back"
                     >> fail "hung"
        Just () -> pure outcome

emptyBatch :: BatchResult
emptyBatch = BatchResult
  { brId        = BatchId "forced"
  , brAt        = UTCTime (fromGregorian 2026 11 10) 0
  , brOperation = OperationId "any"
  , brOptions   = OptionValues M.empty
  , brInput     = MarkdownInput
  , brFiles     = []
  }

oneRow :: BatchFile
oneRow = DocRow "a.md" Unchanged (Just "x\n") Nothing Nothing

spec :: Spec
spec = do
  describe "a run whose operation raises a pure exception" $ do
    it "is rejected with the standard 400 and the form panel, not a 500" $ do
      (res, _) <- runOnce crashing
      simpleStatus res `shouldBe` status400
      contains (TE.encodeUtf8 runFailedMessage) res `shouldBe` True
      countOf "id=\"run-form\"" res `shouldBe` 1

    it "never shows the visitor the exception's own text" $ do
      (res, logged) <- runOnce crashing
      contains (BS8.pack crashText) res `shouldBe` False
      logged `shouldSatisfy` (not . BS.isInfixOf (BS8.pack crashText))

    it "logs a reason of its own" $ do
      (_, logged) <- runOnce crashing
      logged `shouldSatisfy` BS.isInfixOf (logLine ReasonRunFailed)

  describe "a run that outlasts the limit" $ do
    it "is rejected with the standard 400 and a message naming the limit" $ do
      (res, _) <- runOnce runaway
      simpleStatus res `shouldBe` status400
      contains (TE.encodeUtf8 (runTimeoutMessage shortLimit)) res `shouldBe` True
      countOf "id=\"run-form\"" res `shouldBe` 1

    it "logs a reason of its own, distinct from a failed run's" $ do
      (_, logged) <- runOnce runaway
      logged `shouldSatisfy` BS.isInfixOf (logLine ReasonRunTimeout)
      logged `shouldSatisfy` (not . BS.isInfixOf (logLine ReasonRunFailed))

    it "is answered moments after the limit, not when the work would have ended" $ do
      sent <- getCurrentTime
      (res, _) <- runOnce runaway
      answered <- getCurrentTime
      simpleStatus res `shouldBe` status400
      diffUTCTime answered sent `shouldSatisfy` (< 1)

  describe "the reject reason in the log" $ do
    it "names an unknown operation as such, not as an upload" $ do
      app <- appFor crashing
      (res, logged) <- captureStdout $ withApplication app $
        postRun [Field "operation" "no-such-operation", Upload "notes.md" sampleMarkdown]
      simpleStatus res `shouldBe` status400
      contains (TE.encodeUtf8 unknownOperationMessage) res `shouldBe` True
      logged `shouldSatisfy` BS.isInfixOf (logLine ReasonOperation)
      logged `shouldSatisfy` (not . BS.isInfixOf (logLine ReasonUpload))

    it "keeps an upload refusal's reason as upload" $ do
      app <- appFor crashing
      (_, logged) <- captureStdout $ withApplication app $
        postRun [Field "operation" (TE.encodeUtf8 (unOperationId (opId crashing)))]
      logged `shouldSatisfy` BS.isInfixOf (logLine ReasonUpload)

    it "gives every reason its own spelling" $
      let spellings = map rejectReasonText [minBound .. maxBound]
       in length (M.keys (M.fromList [(s, ()) | s <- spellings])) `shouldBe` length spellings

  describe "forceBatch" $ do
    it "hands back an ordinary batch unchanged" $ do
      let batch = emptyBatch { brFiles = [oneRow] }
      forced <- forceBatch 5 batch
      fmap brFiles forced `shouldBe` Right [oneRow]

    it "finds an exception buried in a row's text" $ do
      let poisoned = DocRow "a.md" Unchanged (Just (error crashText)) Nothing Nothing
          batch = emptyBatch { brFiles = [poisoned] }
      forced <- forceBatch 5 batch
      forced `shouldBe` Left RunFailed

    it "does not swallow the cancellation of the thread it runs on" $ do
      let sleeping = emptyBatch { brFiles = unsafePerformIO (threadDelay tenSeconds >> pure []) }
      done <- newEmptyMVar
      thread <- forkFinally (forceBatch 30 sleeping) (putMVar done)
      threadDelay 100000
      killThread thread
      outcome <- takeMVar done
      case outcome of
        Left (e :: SomeException) -> fromException e `shouldBe` Just ThreadKilled
        Right _ -> expectationFailure "the kill was turned into a result"

  describe "a run killed while its body is still arriving" $
    it "stops, rather than answering the kill as an over-limit upload" $ do
      env <- envFor passing id
      answered <- newEmptyMVar
      finished <- newEmptyMVar
      let stalled = setRequestBodyChunks (threadDelay tenSeconds >> pure BS.empty)
            defaultRequest
              { requestMethod  = methodPost
              , pathInfo       = ["run"]
              , requestHeaders = [("Content-Type", multipartContentType)]
              }
          answer response = do
            putMVar answered (responseStatus response)
            pure ResponseReceived
      thread <- forkFinally (application env stalled answer) (putMVar finished)
      threadDelay 100000
      killThread thread
      outcome <- timeout (3 * 1000 * 1000) (takeMVar finished)
      case outcome of
        Just (Left e) -> fromException e `shouldBe` Just ThreadKilled
        Just (Right _) -> do
          status <- takeMVar answered
          expectationFailure ("the kill was answered with " <> show status)
        Nothing -> expectationFailure "the killed run never ended"

  describe "a server at its run cap" $ do
    it "answers the next run with 429, a Retry-After and a plain body, unparsed" $ do
      env <- envFor passing (\c -> c { cfgMaxConcurrentRuns = 1 })
      (res, logged) <- captureStdout . holding (envRunGate env)
                         . withApplication (application env) $
        request methodPost "/run"
          [("Content-Type", "application/json")]
          "{"
      simpleStatus res `shouldBe` status429
      lookup "Retry-After" (simpleHeaders res)
        `shouldBe` Just (BS8.pack (show runBusyRetrySeconds))
      contains (TE.encodeUtf8 runBusyMessage) res `shouldBe` True
      contains "id=\"run-form\"" res `shouldBe` False
      logged `shouldSatisfy` BS.isInfixOf (logLine ReasonBusy)

    it "admits a run again once the permit is back" $ do
      env <- envFor passing (\c -> c { cfgMaxConcurrentRuns = 1 })
      _ <- holding (envRunGate env) (pure ())
      (res, _) <- captureStdout . withApplication (application env) $
        postRun
          [ Field "operation" (TE.encodeUtf8 (unOperationId (opId passing)))
          , Upload "notes.md" sampleMarkdown
          ]
      simpleStatus res `shouldBe` status200

    it "reads a refused upload to its end, so a client still sending gets the 429" $ do
      env <- envFor passing (\c -> c { cfgMaxConcurrentRuns = 1 })
      (answer, _) <- captureStdout . holding (envRunGate env) $
        testWithApplication (pure (application env)) $ \port ->
          rawPost port (16 * 1024 * 1024)
      case answer of
        Left problem -> expectationFailure ("the upload was cut off: " <> problem)
        Right reply -> BS.take 12 reply `shouldBe` "HTTP/1.1 429"

    it "answers a declared body over the ceiling with the 429, not a reset" $ do
      env <- envFor passing (\c -> c { cfgMaxConcurrentRuns = 1, cfgMaxBatchBytes = 10 * 1024 * 1024 })
      (answer, _) <- captureStdout . holding (envRunGate env) $
        testWithApplication (pure (application env)) $ \port ->
          rawPost port (16 * 1024 * 1024)
      case answer of
        Left problem -> expectationFailure ("the upload was cut off: " <> problem)
        Right reply -> BS.take 12 reply `shouldBe` "HTTP/1.1 429"

  describe "a body over the whole-body ceiling, through a real socket" $ do
    it "answers a declared length over it with the 400 banner, not a reset" $ do
      env <- envFor passing (\c -> c { cfgMaxBatchBytes = 10 * 1024 * 1024 })
      (answer, _) <- captureStdout . testWithApplication (pure (application env)) $ \port ->
        rawPost port (16 * 1024 * 1024)
      case answer of
        Left problem -> expectationFailure ("the upload was cut off: " <> problem)
        Right reply -> do
          BS.take 12 reply `shouldBe` "HTTP/1.1 400"
          reply `shouldSatisfy` BS.isInfixOf (TE.encodeUtf8 parseFailureMessage)

    it "answers a chunked body that runs over it with the 400 banner, not a reset" $ do
      env <- envFor passing (\c -> c { cfgMaxBatchBytes = 10 * 1024 * 1024 })
      (answer, _) <- captureStdout . testWithApplication (pure (application env)) $ \port ->
        rawChunkedPost port (16 * 1024 * 1024)
      case answer of
        Left problem -> expectationFailure ("the upload was cut off: " <> problem)
        Right reply -> do
          BS.take 12 reply `shouldBe` "HTTP/1.1 400"
          reply `shouldSatisfy` BS.isInfixOf (TE.encodeUtf8 parseFailureMessage)

  describe "discardBody" $ do
    it "stops reading a refused upload past its bound" $ do
      reads' <- newIORef (0 :: Int)
      let chunk = BS.replicate 4096 0x61
          source = modifyIORef' reads' (+ 1) >> pure chunk
          bound = maxRequestBytes (limitsFromConfig defaultConfig { cfgMaxBatchBytes = 40960 })
      discardBody bound source
      consumed <- readIORef reads'
      (consumed * BS.length chunk) `shouldSatisfy` (<= bound + BS.length chunk)
      (consumed * BS.length chunk) `shouldSatisfy` (>= bound)

    it "stops reading a refused upload at its end" $ do
      remaining <- newIORef [BS.replicate 10 0x61, BS.replicate 10 0x62]
      let source = atomicModifyIORef' remaining $ \cs -> case cs of
            (c : rest) -> (rest, c)
            []         -> ([], BS.empty)
      discardBody maxBound source
      left <- readIORef remaining
      left `shouldBe` []

rawPost :: Int -> Int -> IO (Either String BS.ByteString)
rawPost port size = do
  outcome <- try $ bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
    connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    sendAll s $ BS8.concat
      [ "POST /run HTTP/1.1\r\nHost: localhost\r\n"
      , "Content-Type: ", multipartContentType, "\r\n"
      , "Content-Length: ", BS8.pack (show size), "\r\n"
      , "Connection: close\r\n\r\n"
      ]
    let block = BS.replicate (64 * 1024) 0x61
        go n | n <= 0 = pure ()
             | otherwise = sendAll s (BS.take n block) >> go (n - BS.length block)
    go size
    readAll s
  pure $ case outcome of
    Left (e :: IOException) -> Left (show e)
    Right reply -> Right reply
  where
    readAll s = do
      piece <- recv s 65536
      if BS.null piece then pure BS.empty else (piece <>) <$> readAll s

rawChunkedPost :: Int -> Int -> IO (Either String BS.ByteString)
rawChunkedPost port size = do
  outcome <- try $ bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
    connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    sendAll s $ BS8.concat
      [ "POST /run HTTP/1.1\r\nHost: localhost\r\n"
      , "Content-Type: ", multipartContentType, "\r\n"
      , "Transfer-Encoding: chunked\r\n"
      , "Connection: close\r\n\r\n"
      ]
    let block = BS.replicate (64 * 1024) 0x61
        chunk bytes = BS8.concat [BS8.pack (showHex (BS.length bytes) ""), "\r\n", bytes, "\r\n"]
        go n | n <= 0 = sendAll s "0\r\n\r\n"
             | otherwise = sendAll s (chunk (BS.take n block)) >> go (n - BS.length block)
    go size
    readAll s
  pure $ case outcome of
    Left (e :: IOException) -> Left (show e)
    Right reply -> Right reply
  where
    readAll s = do
      piece <- recv s 65536
      if BS.null piece then pure BS.empty else (piece <>) <$> readAll s
