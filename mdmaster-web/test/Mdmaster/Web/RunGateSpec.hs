module Mdmaster.Web.RunGateSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception
  (MaskingState (..), SomeException, evaluate, getMaskingState, throwIO, try)
import Data.IORef (newIORef, readIORef)
import Data.Time (diffUTCTime, getCurrentTime)
import Mdmaster.Web.RunGate (newRunGate, withRunPermit)
import Mdmaster.Web.TestSupport (onOwnThread, spin, spinSteps)
import System.Timeout (timeout)
import Test.Hspec

type Outcome = String

ran, refused :: IO Outcome
ran     = pure "ran"
refused = pure "refused"

spec :: Spec
spec = do
  describe "withRunPermit" $ do
    it "runs an action that finds a permit free" $ do
      gate <- newRunGate 1
      outcome <- withRunPermit gate refused ran
      outcome `shouldBe` "ran"

    it "refuses an action that finds none free, running the refusal instead" $ do
      gate <- newRunGate 1
      outcome <- withRunPermit gate refused $
                   withRunPermit gate refused ran
      outcome `shouldBe` "refused"

    it "admits as many actions at once as it was built with" $ do
      gate <- newRunGate 2
      outcome <- withRunPermit gate refused $
                   withRunPermit gate refused ran
      outcome `shouldBe` "ran"

    it "has the permit back once the action ends" $ do
      gate <- newRunGate 1
      _ <- withRunPermit gate refused ran
      again <- withRunPermit gate refused ran
      again `shouldBe` "ran"

    it "has the permit back even when the action throws" $ do
      gate <- newRunGate 1
      raised <- try (withRunPermit gate refused
                       (throwIO (userError "the run was cut short")))
                 :: IO (Either SomeException Outcome)
      case raised of
        Left _  -> pure ()
        Right _ -> expectationFailure "the exception was swallowed"
      again <- withRunPermit gate refused ran
      again `shouldBe` "ran"

    it "runs the action with asynchronous exceptions as the caller had them" $ do
      gate <- newRunGate 1
      state <- withRunPermit gate (pure Unmasked) getMaskingState
      state `shouldBe` Unmasked

    it "runs the refusal with asynchronous exceptions as the caller had them" $ do
      gate <- newRunGate 1
      state <- withRunPermit gate (pure MaskedInterruptible) $
                 withRunPermit gate getMaskingState (pure MaskedInterruptible)
      state `shouldBe` Unmasked

    it "lets a time limit stop an action spinning in pure code, and has the permit back" $ do
      gate <- newRunGate 1
      steps <- newIORef spinSteps
      sent <- getCurrentTime
      stopped <- onOwnThread (3 * 1000 * 1000) $
        timeout 100000 $ withRunPermit gate refused $ do
          n <- readIORef steps
          _ <- evaluate (spin n)
          ran
      answered <- getCurrentTime
      stopped `shouldBe` Just Nothing
      diffUTCTime answered sent `shouldSatisfy` (< 1)
      again <- withRunPermit gate refused ran
      again `shouldBe` "ran"

    it "has the permit back when the action's thread is killed" $ do
      gate <- newRunGate 1
      started <- newEmptyMVar
      finished <- newEmptyMVar
      steps <- newIORef spinSteps
      thread <- forkIO $ do
        _ <- try (withRunPermit gate refused $ do
                    putMVar started ()
                    n <- readIORef steps
                    _ <- evaluate (spin n)
                    ran) :: IO (Either SomeException Outcome)
        putMVar finished ()
      takeMVar started
      threadDelay 10000
      killed <- getCurrentTime
      _ <- forkIO (killThread thread)
      gone <- timeout (3 * 1000 * 1000) (takeMVar finished)
      ended <- getCurrentTime
      gone `shouldBe` Just ()
      diffUTCTime ended killed `shouldSatisfy` (< 1)
      again <- withRunPermit gate refused ran
      again `shouldBe` "ran"
