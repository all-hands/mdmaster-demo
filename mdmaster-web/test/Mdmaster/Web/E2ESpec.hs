module Mdmaster.Web.E2ESpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Mdmaster.App.Docx.Admission (DocxAdmissionError (..), admissionMessage)
import Mdmaster.App.FileStore (newInMemoryStoreWith)
import Mdmaster.Core.Operation
  (InputFormat (..), Operation (..), OperationId (..))
import Mdmaster.Plugins.Registry (allOperations)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Limits (acceptedPhrase)
import Mdmaster.Web.ServerSpec
  ( appWith
  , appWithStore
  , batchIdIn
  , contains
  , operationField
  , postRun
  , sampleMarkdown
  )
import Mdmaster.Web.TestSupport (Part (..), corruptDocx, docxFixture)
import Mdmaster.Web.View.Layout (acceptedExtensions)
import Mdmaster.Web.View.Results (expiredHeadline)
import Network.HTTP.Types (status200, status400, status410)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

firstDocument :: BS.ByteString
firstDocument = "# One\n\n## Alpha\n\nbody\n\n## Beta\n\nbody\n"

secondDocument :: BS.ByteString
secondDocument = "# Two\n\n## Gamma\n\nbody\n"

docxOperationId :: OperationId
docxOperationId = case filter ((== DocxInput) . opInput) allOperations of
  (operation : _) -> opId operation
  []              -> error "no operation in the registry reads docx"

docxOperationField :: Part
docxOperationField = Field "operation" (TE.encodeUtf8 (unOperationId docxOperationId))

docxExts :: BS.ByteString
docxExts = TE.encodeUtf8 (acceptedExtensions DocxInput)

expiringApp :: IO (IORef UTCTime, Application)
expiringApp = do
  start <- getCurrentTime
  clock <- newIORef start
  store <- newInMemoryStoreWith
    (readIORef clock) (cfgResultTtl defaultConfig) (cfgMaxResults defaultConfig)
    (cfgMaxStoreBytes defaultConfig)
  app <- appWithStore defaultConfig store
  pure (clock, app)

spec :: Spec
spec = do
  with (appWith defaultConfig) $
    describe "one visit, start to finish" $
      it "uploads two files, reads both diffs and downloads the zip" $ do
        run <- postRun
          [ operationField
          , Upload "one.md" firstDocument
          , Upload "docs/two.md" secondDocument
          ]
        liftIO $ do
          simpleStatus run `shouldBe` status200
          contains "one.md" run `shouldBe` True
          contains "docs/two.md" run `shouldBe` True

        let bid = batchIdIn run
        liftIO $ BS.null bid `shouldBe` False

        firstDiff <- get ("/batch/" <> bid <> "/file/0/diff")
        liftIO $ do
          simpleStatus firstDiff `shouldBe` status200
          contains "one.md" firstDiff `shouldBe` True
          contains "Alpha" firstDiff `shouldBe` True

        secondDiff <- get ("/batch/" <> bid <> "/file/1/diff")
        liftIO $ do
          simpleStatus secondDiff `shouldBe` status200
          contains "docs/two.md" secondDiff `shouldBe` True
          contains "Gamma" secondDiff `shouldBe` True

        archive <- get ("/batch/" <> bid <> "/download")
        liftIO $ do
          simpleStatus archive `shouldBe` status200
          BS.isPrefixOf "PK" (BL.toStrict (simpleBody archive)) `shouldBe` True
          contains "one.md" archive `shouldBe` True
          contains "docs/two.md" archive `shouldBe` True

  with (appWith defaultConfig) $
    describe "a visit that uploads a Word document" $
      it "accepts a .docx and hands back Markdown under a .md name" $ do
        bytes <- liftIO (BS.readFile docxFixture)
        run <- postRun [docxOperationField, Upload "notes.docx" bytes]
        liftIO $ do
          simpleStatus run `shouldBe` status200
          contains "notes.md" run `shouldBe` True
          contains "notes.docx" run `shouldBe` False
          contains "media/image1.png" run `shouldBe` True
          contains "/diff" run `shouldBe` False
          contains "id=\"diff-pane\"" run `shouldBe` False

        let bid = batchIdIn run
        download <- get ("/batch/" <> bid <> "/file/0/download")
        liftIO $ do
          simpleStatus download `shouldBe` status200
          lookup "Content-Disposition" (simpleHeaders download)
            `shouldBe` Just "attachment; filename=\"notes.md\""
          contains "Intro sentence before any comment." download `shouldBe` True

        image <- get ("/batch/" <> bid <> "/file/1/download")
        liftIO $ do
          simpleStatus image `shouldBe` status200
          lookup "Content-Disposition" (simpleHeaders image)
            `shouldBe` Just "attachment; filename=\"media/image1.png\""
          BS.isPrefixOf "\x89PNG" (BL.toStrict (simpleBody image)) `shouldBe` True

        archive <- get ("/batch/" <> bid <> "/download")
        liftIO $ do
          simpleStatus archive `shouldBe` status200
          contains "notes.md" archive `shouldBe` True
          contains "notes.docx" archive `shouldBe` False
          contains "media/image1.png" archive `shouldBe` True

  with (appWith defaultConfig) $
    describe "a visit that uploads a Word document with a corrupt part" $
      it "explains it rather than answering 500" $ do
        run <- postRun [docxOperationField, Upload "notes.docx" corruptDocx]
        liftIO $ do
          simpleStatus run `shouldBe` status400
          contains (TE.encodeUtf8 (admissionMessage (CorruptDocxStream ""))) run
            `shouldBe` True

  with (appWith defaultConfig { cfgDefaultOperation = docxOperationId }) $
    describe "a page whose default operation reads Word documents" $
      it "says so before anything has been uploaded or chosen" $ do
        res <- get "/"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          contains ("data-accept-extensions=\"" <> docxExts <> "\"") res
            `shouldBe` True
          contains ("accept=\"" <> docxExts <> "\"") res `shouldBe` True
          contains (TE.encodeUtf8 (">" <> acceptedPhrase DocxInput
                     <> "</span> files are uploaded")) res `shouldBe` True

  withState expiringApp $
    describe "the same batch once its time to live has run out" $
      it "stops serving it and explains that the result expired" $ do
        run <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        liftIO $ simpleStatus run `shouldBe` status200
        let bid = batchIdIn run

        fresh <- get ("/batch/" <> bid <> "/download")
        liftIO $ simpleStatus fresh `shouldBe` status200

        clock <- getState
        liftIO $ modifyIORef' clock (addUTCTime (cfgResultTtl defaultConfig + 1))

        stale <- get ("/batch/" <> bid <> "/download")
        liftIO $ do
          simpleStatus stale `shouldBe` status410
          contains (TE.encodeUtf8 expiredHeadline) stale `shouldBe` True
