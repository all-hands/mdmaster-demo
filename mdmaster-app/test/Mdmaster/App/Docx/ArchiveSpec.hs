module Mdmaster.App.Docx.ArchiveSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Mdmaster.App.Docx.Archive (DocxParts (..), docxParts)
import Mdmaster.App.TestSupport (zipOf)
import Test.Hspec

fixturePath :: FilePath
fixturePath = "test/fixtures/comments.docx"

fixture :: IO BL.ByteString
fixture = BL.readFile fixturePath

shouldContainText :: Maybe BL.ByteString -> TL.Text -> Expectation
shouldContainText Nothing marker =
  expectationFailure ("part is absent; expected it to contain " <> show marker)
shouldContainText (Just bytes) marker =
  TLE.decodeUtf8 bytes `shouldSatisfy` TL.isInfixOf marker

spec :: Spec
spec = do
  describe "docxParts" $ do
    it "finds all four comment metadata parts in the fixture" $ do
      bytes <- fixture
      case docxParts bytes of
        Left err -> expectationFailure (T.unpack err)
        Right parts ->
          map (fmap (const ()))
            [ dpComments parts
            , dpCommentsExtended parts
            , dpCommentsIds parts
            , dpCommentsExtensible parts
            ]
            `shouldBe` [Just (), Just (), Just (), Just ()]

    it "gives each field the bytes of the part it names" $ do
      bytes <- fixture
      case docxParts bytes of
        Left err -> expectationFailure (T.unpack err)
        Right parts -> do
          dpComments parts `shouldContainText` "<w:comments "
          dpCommentsExtended parts `shouldContainText` "<w15:commentsEx "
          dpCommentsIds parts `shouldContainText` "<w16cid:commentsIds "
          dpCommentsExtensible parts
            `shouldContainText` "<w16cex:commentsExtensible "

    it "reports every part absent for a document with no comments" $
      docxParts (zipOf [("word/document.xml", "<w:document/>")])
        `shouldBe` Right (DocxParts Nothing Nothing Nothing Nothing)

    it "rejects bytes that are not a zip at all" $
      docxParts "this is not a zip file" `shouldSatisfy` isLeft
