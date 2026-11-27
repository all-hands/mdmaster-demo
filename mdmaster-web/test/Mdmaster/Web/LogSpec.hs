module Mdmaster.Web.LogSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Types (BatchId (..))
import Mdmaster.Core.Operation (OperationId (..))
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Identity (IdentityProvider (..), anonymousProvider)
import Mdmaster.Web.Log
  (RejectReason (..), batchTag, logEvent, logReject, logRun, rejectReasonText)
import Mdmaster.Web.ServerSpec (appWith, batchIdIn, operationField, postRun)
import Mdmaster.Web.TestSupport (Part (..), captureStdout)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)

logged :: IO () -> IO Text
logged action = TE.decodeUtf8 . snd <$> captureStdout action

anonymousLabel :: IO Text
anonymousLabel = resolveIdentity anonymousProvider []

runLine :: Rational -> IO Text
runLine elapsed = do
  label <- anonymousLabel
  logged $ logRun label (BatchId "b1") (OperationId "op") 2 (fromRational elapsed)

msOf :: Text -> Text
msOf line = T.takeWhile (/= ' ') (snd (T.breakOnEnd " ms=" (T.stripEnd line)))

spec :: Spec
spec = do
  describe "logEvent" $ do
    it "writes the event alone as one line" $
      logged (logEvent "ping" []) `shouldReturn` "event=ping\n"

    it "puts the event first, then the fields in the order given, one space apart" $
      logged (logEvent "ping" [("b", "2"), ("a", "1")])
        `shouldReturn` "event=ping b=2 a=1\n"

    it "writes one line per call" $
      T.lines <$> logged (logEvent "one" [] >> logEvent "two" [("k", "v")])
        `shouldReturn` ["event=one", "event=two k=v"]

  describe "batchTag" $ do
    it "is the first 8 hex digits of the id's SHA-256" $ do
      batchTag (BatchId "abc123") `shouldBe` "6ca13d52"
      batchTag (BatchId "0123456789abcdef0123456789abcdef") `shouldBe` "3eb1bd43"

    it "never contains the id itself" $ do
      let bid = "0123456789abcdef0123456789abcdef"
      batchTag (BatchId bid) `shouldSatisfy` (not . (`T.isInfixOf` bid))
      T.length (batchTag (BatchId bid)) `shouldBe` 8

  describe "logRun" $ do
    it "writes the event, then identity, batch, operation, files and ms, in that order" $ do
      label <- anonymousLabel
      line <- logged (logRun label (BatchId "abc123") (OperationId "toc") 3 0.25)
      line `shouldBe`
        "event=run identity=" <> label <> " batch=" <> batchTag (BatchId "abc123")
          <> " operation=toc files=3 ms=250\n"

    it "attributes the run to the label the identity provider resolved" $ do
      line <- logged (logRun "someone-else" (BatchId "b") (OperationId "op") 1 0)
      line `shouldSatisfy` T.isInfixOf " identity=someone-else "

    it "counts files, not bytes or paths" $ do
      line <- logged (logRun "x" (BatchId "b") (OperationId "op") 17 0)
      line `shouldSatisfy` T.isInfixOf " files=17 "

    describe "the ms field" $ do
      it "reports whole seconds as thousands" $
        (msOf <$> runLine 1.5) `shouldReturn` "1500"

      it "reports a quarter second as 250" $
        (msOf <$> runLine 0.25) `shouldReturn` "250"

      it "reports no time as 0" $
        (msOf <$> runLine 0) `shouldReturn` "0"

      it "rounds below half a millisecond down" $
        (msOf <$> runLine 0.0004) `shouldReturn` "0"

      it "rounds above half a millisecond up" $
        (msOf <$> runLine 0.0006) `shouldReturn` "1"

      it "prints an integer, never a fraction or an exponent" $ do
        ms <- msOf <$> runLine 0.123456
        ms `shouldBe` "123"
        T.all (`elem` ['0' .. '9']) ms `shouldBe` True

  describe "logReject" $ do
    it "writes exactly the event and the reason, for every reason" $
      mapM_
        (\reason -> logged (logReject reason)
                      `shouldReturn` "event=reject reason=" <> rejectReasonText reason <> "\n")
        [minBound .. maxBound :: RejectReason]

    it "writes one line per rejection" $
      length . T.lines <$> logged (mapM_ logReject [minBound .. maxBound :: RejectReason])
        `shouldReturn` length [minBound .. maxBound :: RejectReason]

  describe "a served run" $ do
    let secretName, secretContent :: BS.ByteString
        secretName    = "private-quarterly-numbers.md"
        secretContent = "# Sensitive heading\n\nclassified paragraph\n"

    it "logs one run line carrying the tag of the batch id the response publishes" $ do
      app <- appWith defaultConfig
      label <- anonymousLabel
      (res, out) <- captureStdout $ withApplication app $
        postRun [operationField, Upload secretName secretContent]
      let line   = TE.decodeUtf8 out
          bid    = TE.decodeUtf8 (batchIdIn res)
          opText = unOperationId (cfgDefaultOperation defaultConfig)
      T.lines line `shouldSatisfy` ((== 1) . length)
      line `shouldSatisfy` T.isPrefixOf
        ("event=run identity=" <> label <> " batch=" <> batchTag (BatchId bid) <> " ")
      line `shouldSatisfy` T.isInfixOf (" operation=" <> opText <> " files=1 ms=")

    it "leaves the batch id itself out of the line" $ do
      app <- appWith defaultConfig
      (res, out) <- captureStdout $ withApplication app $
        postRun [operationField, Upload secretName secretContent]
      BS.length (batchIdIn res) `shouldBe` 32
      out `shouldSatisfy` (not . BS.isInfixOf (batchIdIn res))

    it "leaves the file's name and contents out of the line" $ do
      app <- appWith defaultConfig
      (_, out) <- captureStdout $ withApplication app $
        postRun [operationField, Upload secretName secretContent]
      out `shouldSatisfy` (not . BS.isInfixOf secretName)
      out `shouldSatisfy` (not . BS.isInfixOf "classified")
      out `shouldSatisfy` (not . BS.isInfixOf "Sensitive")

