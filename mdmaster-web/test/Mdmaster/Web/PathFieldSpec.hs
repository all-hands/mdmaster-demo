module Mdmaster.Web.PathFieldSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text.Encoding as TE
import Mdmaster.App.FileStore (FileStore (..), newInMemoryStore)
import Mdmaster.App.Types (BatchId (..), BatchResult (..))
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Limits (UploadError (..), pathFieldName, uploadErrorMessage)
import Mdmaster.Web.ServerSpec
  (appWithStore, batchIdIn, contains, operationField, postRun, sampleMarkdown)
import Mdmaster.Web.TestSupport (Part (..), captureStdout, frPath)
import Network.HTTP.Types (status200, status400)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)

run :: [Part] -> IO (SResponse, [FilePath], BS.ByteString)
run parts = do
  let config = defaultConfig
  store <- newInMemoryStore (cfgResultTtl config) (cfgMaxResults config) (cfgMaxStoreBytes config)
  app <- appWithStore config store
  (res, logged) <- captureStdout (withApplication app (postRun (operationField : parts)))
  stored <- fsGet store (BatchId (TE.decodeUtf8 (batchIdIn res)))
  pure (res, maybe [] (sort . map frPath . brFiles) stored, logged)

path :: BS.ByteString -> Part
path = Field pathFieldName

spec :: Spec
spec = describe "a file's path field" $ do
  it "names the file, as the page sent it, over the altered multipart filename" $ do
    (res, paths, _) <- run
      [ Upload "FolderD/say %22hi%22.md" sampleMarkdown, path "FolderD/say \"hi\".md"
      , Upload "FolderD/plain.md" sampleMarkdown,        path "FolderD/plain.md"
      ]
    simpleStatus res `shouldBe` status200
    paths `shouldBe` ["FolderD/plain.md", "FolderD/say \"hi\".md"]

  it "is paired with the files in order, whatever the filenames say" $ do
    (res, paths, _) <- run
      [ Upload "one.md" sampleMarkdown, path "docs/first.md"
      , Upload "two.md" sampleMarkdown, path "docs/second.md"
      ]
    simpleStatus res `shouldBe` status200
    paths `shouldBe` ["docs/first.md", "docs/second.md"]

  it "is optional: without one, the multipart filenames are the names" $ do
    (res, paths, _) <- run
      [ Upload "notes/a.md" sampleMarkdown, Upload "notes/b.md" sampleMarkdown ]
    simpleStatus res `shouldBe` status200
    paths `shouldBe` ["notes/a.md", "notes/b.md"]

  it "refuses a count that does not match the files, rather than guessing" $ do
    (res, paths, logged) <- run
      [ Upload "secret-one.md" sampleMarkdown, path "secret-one.md"
      , Upload "secret-two.md" sampleMarkdown
      ]
    simpleStatus res `shouldBe` status400
    contains (TE.encodeUtf8 (uploadErrorMessage (PathCountMismatch 1 2))) res `shouldBe` True
    paths `shouldBe` []
    logged `shouldSatisfy` BS.isInfixOf "event=reject reason=upload"
    logged `shouldSatisfy` (not . BS.isInfixOf "secret")

  it "brings a backslash to the path rule, which refuses it with the reason" $ do
    (res, paths, _) <- run
      [ Upload "FolderE/backslash.md" sampleMarkdown, path "FolderE/back\\slash.md" ]
    simpleStatus res `shouldBe` status400
    contains "is not a safe relative path" res `shouldBe` True
    contains "backslashes, line breaks" res `shouldBe` True
    paths `shouldBe` []

  it "refuses a line break in a path with the reason, rather than renaming the file" $ do
    (res, paths, logged) <- run
      [ Upload "FolderD/two%0Alines.md" sampleMarkdown, path "FolderD/two\r\nlines.md" ]
    simpleStatus res `shouldBe` status400
    contains "is not a safe relative path" res `shouldBe` True
    paths `shouldBe` []
    logged `shouldSatisfy` (not . BS.isInfixOf "lines.md")

  it "is not taken for one of the operation's options" $ do
    (res, _, _) <- run [ Upload "a.md" sampleMarkdown, path "a.md" ]
    simpleStatus res `shouldBe` status200
    contains "name=\"paths\"" res `shouldBe` False
