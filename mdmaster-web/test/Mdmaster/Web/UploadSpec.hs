module Mdmaster.Web.UploadSpec (spec) where

import Control.Exception (ErrorCall (..), try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Docx.Admission (DocxAdmissionError (..), admissionMessage)
import Mdmaster.App.Types (DocSource (..), Document (..))
import Mdmaster.Core.Operation (InputFormat (..))
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Limits
import Mdmaster.Web.TestSupport (capped, corruptDocx, craftedDocx, docxFixture, md, notWordDocx)
import Mdmaster.Web.Upload (extractDocuments, extractDocumentsIO)
import Test.Hspec

body :: BS.ByteString
body = TE.encodeUtf8 "# Title\n"

small :: Config
small = defaultConfig { cfgMaxFiles = 3, cfgMaxFileBytes = 100, cfgMaxBatchBytes = 200 }


data DocSummary = DocSummary FilePath String Int
  deriving (Eq, Show)

summarise :: Document -> DocSummary
summarise doc = case docSource doc of
  MarkdownSource content -> DocSummary (docPath doc) "markdown" (T.length content)
  DocxSource bytes       -> DocSummary (docPath doc) "docx" (BS.length bytes)

showSummarised :: Either UploadError [Document] -> String
showSummarised = show . fmap (map summarise)

spec :: Spec
spec = do
  describe "extractDocuments" $ do
    it "keeps Markdown files and decodes their content" $
      extractDocuments small MarkdownInput [md "a.md" body]
        `shouldBe` Right [Document "a.md" (MarkdownSource "# Title\n")]

    it "drops non-Markdown files silently" $
      extractDocuments small MarkdownInput [md "a.md" body, md "logo.png" "\xff\xd8"]
        `shouldBe` Right [Document "a.md" (MarkdownSource "# Title\n")]

    it "preserves upload order" $
      fmap (map docPath)
        (extractDocuments small MarkdownInput [md "b.md" body, md "a.md" body])
        `shouldBe` Right ["b.md", "a.md"]

    it "preserves folder structure" $
      fmap (map docPath) (extractDocuments small MarkdownInput [md "docs/a.md" body])
        `shouldBe` Right ["docs/a.md"]

    it "rejects a file over the per-file limit" $
      extractDocuments small MarkdownInput [md "big.md" (BS.replicate 200 65)]
        `shouldBe` Left (FileTooLarge "big.md" 200 100)

    it "rejects a file that is not valid UTF-8" $
      extractDocuments small MarkdownInput [md "a.md" (BS.pack [0xff, 0xfe, 0xfd])]
        `shouldBe` Left (NotUtf8 "a.md")

    it "accepts non-ASCII UTF-8 content" $
      extractDocuments small MarkdownInput [md "a.md" (TE.encodeUtf8 "# Καλημέρα\n")]
        `shouldBe` Right [Document "a.md" (MarkdownSource "# Καλημέρα\n")]

  describe "docx uploads" $ do
    it "keeps docx bytes undecoded, since a zip is not UTF-8" $ do
      bytes <- BS.readFile docxFixture
      let extracted = extractDocuments defaultConfig DocxInput [md "notes.docx" bytes]
      fmap (map summarise) extracted
        `shouldBe` Right [DocSummary "notes.docx" "docx" (BS.length bytes)]
      (extracted == Right [Document "notes.docx" (DocxSource bytes)]) `shouldBe` True

    it "rejects a file that is not a readable docx" $
      case extractDocuments defaultConfig DocxInput [md "notes.docx" "not a zip"] of
        Left (DocxRejected (NotReadableDocx path _)) -> path `shouldBe` "notes.docx"
        other -> expectationFailure ("expected NotReadableDocx, got " <> showSummarised other)

    it "rejects a valid zip that is not a Word document" $ do
      extractDocuments defaultConfig DocxInput [md "notes.docx" notWordDocx]
        `shouldBe` Left (DocxRejected (NotWordDocx "notes.docx"))
      uploadErrorMessage (DocxRejected (NotWordDocx "notes.docx"))
        `shouldBe` admissionMessage (NotWordDocx "notes.docx")

    it "rejects a docx whose expanded size is over the limit" $ do
      bytes <- BS.readFile docxFixture
      let tiny = defaultConfig { cfgMaxDocxUncompressedBytes = 10 }
      case extractDocuments tiny DocxInput [md "notes.docx" bytes] of
        Left (DocxRejected (DeclaresTooManyBytes path _ limit)) ->
          (path, limit) `shouldBe` ("notes.docx", 10)
        other -> expectationFailure ("expected DeclaresTooManyBytes, got " <> showSummarised other)

    it "accepts the same docx under the default limit" $ do
      bytes <- BS.readFile docxFixture
      fmap (map docPath) (extractDocuments defaultConfig DocxInput [md "notes.docx" bytes])
        `shouldBe` Right ["notes.docx"]

    it "drops a docx when the operation reads Markdown" $
      extractDocuments small MarkdownInput [md "notes.docx" body]
        `shouldBe` Left (NoAcceptableFiles MarkdownInput)

    it "drops a Markdown file when the operation reads docx" $
      extractDocuments small DocxInput [md "a.md" body]
        `shouldBe` Left (NoAcceptableFiles DocxInput)

    it "names the accepted extensions in the no-files message" $
      uploadErrorMessage (NoAcceptableFiles DocxInput)
        `shouldSatisfy` T.isInfixOf ".docx"

    it "still names Markdown extensions for a Markdown operation" $
      uploadErrorMessage (NoAcceptableFiles MarkdownInput)
        `shouldSatisfy` T.isInfixOf ".md"

  describe "docx uploads that lie about their size" $ do
    it "rejects a crafted docx the declared-size check waves through" $
      case extractDocuments (capped 1 100) DocxInput [md "notes.docx" (craftedDocx 4)] of
        Left (DocxRejected (ExpandsTooFar path reason)) -> do
          path `shouldBe` "notes.docx"
          reason `shouldSatisfy` T.isInfixOf "1048576"
        other -> expectationFailure ("expected ExpandsTooFar, got " <> showSummarised other)

    it "accepts the same crafted docx when the cap is above what it expands to" $
      fmap (map docPath)
        (extractDocuments (capped 8 100) DocxInput [md "notes.docx" (craftedDocx 4)])
        `shouldBe` Right ["notes.docx"]

    it "rejects a batch whose docx files pass one by one and not together" $
      case extractDocuments (capped 3 3) DocxInput
             [md "a.docx" (craftedDocx 2), md "b.docx" (craftedDocx 2)] of
        Left err ->
          err `shouldBe` DocxRejected (BatchExpandsTooFar (3 * 1024 * 1024))
        other -> expectationFailure ("expected BatchExpandsTooFar, got " <> showSummarised other)

    it "accepts that same batch under a batch cap that fits it" $
      fmap (map docPath)
        (extractDocuments (capped 3 5) DocxInput
          [md "a.docx" (craftedDocx 2), md "b.docx" (craftedDocx 2)])
        `shouldBe` Right ["a.docx", "b.docx"]

    it "blames the file, not the batch, when the file alone is too large" $
      case extractDocuments (capped 1 100) DocxInput
             [md "a.docx" (craftedDocx 2), md "b.docx" (craftedDocx 2)] of
        Left (DocxRejected (ExpandsTooFar path _)) -> path `shouldBe` "a.docx"
        other -> expectationFailure ("expected ExpandsTooFar, got " <> showSummarised other)

    it "leaves the honest fixture accepted under the default caps" $ do
      bytes <- BS.readFile docxFixture
      fmap (map docPath) (extractDocuments defaultConfig DocxInput [md "notes.docx" bytes])
        `shouldBe` Right ["notes.docx"]

  describe "extractDocumentsIO" $ do
    it "turns a corrupt deflate stream into a value instead of an exception" $ do
      result <- extractDocumentsIO defaultConfig DocxInput [md "notes.docx" corruptDocx]
      case result of
        Left (DocxRejected (CorruptDocxStream reason)) ->
          reason `shouldBe` "premature end of compressed data stream"
        other -> expectationFailure ("expected CorruptDocxStream, got " <> showSummarised other)

    it "hands back what the pure function returns when nothing raises" $ do
      bytes <- BS.readFile docxFixture
      result <- extractDocumentsIO defaultConfig DocxInput [md "notes.docx" bytes]
      fmap (map summarise) result
        `shouldBe` Right [DocSummary "notes.docx" "docx" (BS.length bytes)]

    it "leaves an ordinary rejection as the error it already was" $ do
      result <- extractDocumentsIO small MarkdownInput [md "big.md" (BS.replicate 200 65)]
      result `shouldBe` Left (FileTooLarge "big.md" 200 100)

    it "does not dress a Markdown failure up as a corrupt .docx" $ do
      caught <- try (extractDocumentsIO defaultConfig MarkdownInput
                       [md "notes.md" (error "raised outside the docx arm")])
      case caught of
        Left (ErrorCall message) -> message `shouldBe` "raised outside the docx arm"
        Right result ->
          expectationFailure ("expected the ErrorCall to escape, got " <> showSummarised result)

    it "still rejects a crafted docx, which is a value and not an exception" $ do
      result <- extractDocumentsIO (capped 1 100) DocxInput
                  [md "notes.docx" (craftedDocx 4)]
      case result of
        Left (DocxRejected (ExpandsTooFar path _)) -> path `shouldBe` "notes.docx"
        other -> expectationFailure ("expected ExpandsTooFar, got " <> showSummarised other)

  describe "uploadErrorMessage" $ do
    it "names the offending file for a size failure" $
      uploadErrorMessage (FileTooLarge "big.md" 200 100)
        `shouldBe` "big.md is 200 bytes, over the 100 byte per-file limit"
    it "explains an empty batch" $
      uploadErrorMessage (NoAcceptableFiles MarkdownInput)
        `shouldBe` "No usable files were uploaded. Only .md and .markdown files are accepted."
    it "explains a file count failure" $
      uploadErrorMessage (TooManyFiles 600 500)
        `shouldBe` "600 files uploaded, over the limit of 500"
    it "explains a batch size failure" $
      uploadErrorMessage (BatchTooLarge 300 200)
        `shouldBe` "The upload totals 300 bytes, over the 200 byte limit"
    it "explains an unsafe path without echoing it as markup" $
      uploadErrorMessage (UnsafePath "../a.md")
        `shouldBe` "The path \"../a.md\" is not a safe relative path. \
                   \Names may not start with a slash or contain \"..\" segments, \
                   \backslashes, line breaks or other control characters; \
                   \rename the file and upload it again."
    it "makes a line break in an unsafe path visible rather than breaking the banner" $
      uploadErrorMessage (UnsafePath "FolderD/two\r\nlines.md")
        `shouldSatisfy` T.isInfixOf "\"FolderD/two\\r\\nlines.md\""
    it "explains a path count that does not match the files" $
      uploadErrorMessage (PathCountMismatch 1 2)
        `shouldBe` "The upload named 1 paths for 2 files, so the files could not \
                   \be matched to their names. Reload the page and try again."
    it "explains an over-long path without quoting it" $
      uploadErrorMessage (PathTooLong 2000 1024)
        `shouldBe` "A file path is 2000 bytes long, over the limit of 1024 bytes"
    it "explains an encoding failure" $
      uploadErrorMessage (NotUtf8 "a.md")
        `shouldBe` "a.md is not valid UTF-8 text"

  describe "path safety, through the scan" $
    it "rejects a traversal hidden behind a ./ segment" $
      extractDocuments small MarkdownInput [md "docs/./../a.md" body]
        `shouldBe` Left (UnsafePath "docs/./../a.md")

  describe "server-side extension re-checking" $
    it "is not fooled by a doubled extension" $
      extractDocuments small MarkdownInput [md "a.md.txt" body]
        `shouldBe` Left (NoAcceptableFiles MarkdownInput)

