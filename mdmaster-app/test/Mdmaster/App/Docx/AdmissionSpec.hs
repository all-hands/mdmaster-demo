module Mdmaster.App.Docx.AdmissionSpec (spec) where

import Codec.Archive.Zip
  (Entry (..), addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Text as T
import Mdmaster.App.Docx.Admission
  (DocxAdmissionError (..), DocxLimits (..), admissionMessage, admitDocx, admitDocxIO)
import Mdmaster.App.TestSupport (zipOf)
import Test.Hspec

fixturePath :: FilePath
fixturePath = "test/fixtures/comments.docx"

fixture :: IO BL.ByteString
fixture = BL.readFile fixturePath

generous :: DocxLimits
generous = limits (256 * 1024 * 1024) (1024 * 1024 * 1024)

limits :: Int -> Int -> DocxLimits
limits file batch = DocxLimits { dlFileBytes = file, dlBatchBytes = batch }

generousBatchCap :: Int
generousBatchCap = 1024 * 1024 * 1024

zeros :: Int -> BL.ByteString
zeros mb = BL.replicate (fromIntegral mb * 1024 * 1024) 0

understating :: [(FilePath, BL.ByteString)] -> BL.ByteString
understating = fromArchive . foldr add emptyArchive
  where
    add (path, content) =
      addEntryToArchive (toEntry path 0 content) { eUncompressedSize = 1 }

truncatedStream :: BL.ByteString
truncatedStream = fromArchive (addEntryToArchive cut emptyArchive)
  where
    whole = toEntry "word/document.xml" 0 (zeros 8)
    kept = BL.take (BL.length (eCompressedData whole) - 64) (eCompressedData whole)
    cut = whole
      { eCompressedData   = kept
      , eCompressedSize   = fromIntegral (BL.length kept)
      , eUncompressedSize = 1
      }

documentPart :: BL.ByteString
documentPart = "<w:document/>"

spec :: Spec
spec = do
  describe "admitDocx" $ do
    it "admits the fixture, whose expanded size is over its compressed size" $ do
      bytes <- fixture
      case admitDocx generous 0 "notes.docx" bytes of
        Left err -> expectationFailure (T.unpack (admissionMessage err))
        Right n  -> n `shouldSatisfy` (> fromIntegral (BL.length bytes))

    it "measures what a crafted archive really expands to" $
      admitDocx generous 0 "notes.docx"
        (understating [("word/document.xml", zeros 4)])
        `shouldBe` Right (4 * 1024 * 1024)

    it "rejects a crafted archive that the declared size waves through" $
      let crafted = understating
            (("word/document.xml", documentPart)
               : [("word/part" <> show i <> ".xml", zeros 4) | i <- [0 :: Int .. 3]])
      in admitDocx (limits (1024 * 1024) (1024 * 1024 * 1024)) 0 "notes.docx" crafted
           `shouldBe` Left (ExpandsTooFar "notes.docx"
                             "expands past the 1048576 byte limit when decompressed")

    it "sums across entries rather than capping each one alone" $ do
      let crafted = understating
            [ ("word/document.xml", documentPart)
            , ("word/a.xml", zeros 2)
            , ("word/b.xml", zeros 2)
            ]
      admitDocx (limits (4 * 1024 * 1024) generousBatchCap) 0 "notes.docx" crafted
        `shouldBe` Left (ExpandsTooFar "notes.docx"
                          "expands past the 4194304 byte limit when decompressed")
      admitDocx (limits (4 * 1024 * 1024 + 13) generousBatchCap) 0 "notes.docx" crafted
        `shouldBe` Right (4 * 1024 * 1024 + 13)

    it "accepts a total exactly at the limit and rejects one byte more" $ do
      let content = BL.replicate 100 65
          honest = zipOf [("word/document.xml", content)]
      admitDocx (limits 100 generousBatchCap) 0 "notes.docx" honest
        `shouldBe` Right 100
      admitDocx (limits 99 generousBatchCap) 0 "notes.docx" honest
        `shouldSatisfy` isLeft

    it "counts a media entry toward the measured expansion like any other" $ do
      let media = zipOf
            [ ("word/document.xml", documentPart)
            , ("word/media/image1.png", zeros 4)
            ]
      admitDocx generous 0 "notes.docx" media
        `shouldBe` Right (4 * 1024 * 1024 + 13)
      admitDocx (limits (4 * 1024 * 1024) generousBatchCap) 0 "notes.docx" media
        `shouldSatisfy` isLeft

    it "says what the failure was, without naming the file" $
      case admitDocx (limits 10 generousBatchCap) 0 "notes.docx"
                 (understating [("word/document.xml", zeros 1)]) of
        Right n  -> expectationFailure ("expected a rejection, got " <> show n)
        Left (ExpandsTooFar _ reason) ->
          reason `shouldBe` "expands past the 10 byte limit when decompressed"
        Left err -> expectationFailure ("expected ExpandsTooFar, got " <> show err)

    it "stops inside an oversized entry instead of inflating it whole" $
      admitDocx (limits 1024 generousBatchCap) 0 "notes.docx" truncatedStream
        `shouldSatisfy` isLeft

    it "refuses bytes that are not a zip at all" $
      admitDocx generous 0 "notes.docx" "this is not a zip file"
        `shouldSatisfy` isLeft

  describe "the order admission checks in" $ do
    it "refuses an unreadable zip before anything else is asked of it" $
      case admitDocx (limits 0 0) 0 "notes.docx" "this is not a zip file" of
        Left (NotReadableDocx "notes.docx" _) -> pure ()
        other -> expectationFailure ("expected NotReadableDocx, got " <> show other)

    it "rejects an over-declared file before asking if it is a Word document" $
      let lying = fromArchive
            (addEntryToArchive
               (toEntry "hello.txt" 0 (BL.replicate 10 0)) { eUncompressedSize = 1000 }
               emptyArchive)
      in admitDocx (limits 500 generousBatchCap) 0 "notes.docx" lying
           `shouldBe` Left (DeclaresTooManyBytes "notes.docx" 1000 500)

    it "refuses a valid zip that is not a Word document" $
      admitDocx generous 0 "notes.docx" (zipOf [("hello.txt", "not a Word document")])
        `shouldBe` Left (NotWordDocx "notes.docx")

  describe "the file-versus-batch tie-break" $ do
    let bomb = understating [("word/document.xml", zeros 2)]
        mb   = 1024 * 1024

    it "blames the file when the per-file cap is the nearer cap" $
      admitDocx (limits mb mb) 0 "a.docx" bomb
        `shouldBe` Left (ExpandsTooFar "a.docx"
                          "expands past the 1048576 byte limit when decompressed")

    it "blames the batch when the remainder is the nearer cap" $
      admitDocx (limits mb mb) (512 * 1024) "b.docx" bomb
        `shouldBe` Left (BatchExpandsTooFar mb)

  describe "admissionMessage" $ do
    it "explains an unreadable docx by its reason alone, not twice over" $
      admissionMessage (NotReadableDocx "notes.docx" "not a readable .docx: not a zip file")
        `shouldBe` "notes.docx: not a readable .docx: not a zip file"

    it "explains a docx whose declared size is too large" $
      admissionMessage (DeclaresTooManyBytes "notes.docx" 900 100)
        `shouldBe` "notes.docx declares 900 bytes, over the 100 byte limit"

    it "names the missing document part for a zip that is not a Word document" $
      admissionMessage (NotWordDocx "notes.docx")
        `shouldBe` "notes.docx: not a Word document: word/document.xml is missing"

    it "explains a docx whose measured expansion is too large" $
      admissionMessage
        (ExpandsTooFar "notes.docx" "expands past the 100 byte limit when decompressed")
        `shouldBe` "notes.docx: expands past the 100 byte limit when decompressed"

    it "explains a batch that expands too far, without inventing a total" $
      admissionMessage (BatchExpandsTooFar 200)
        `shouldBe` "The .docx files in this upload expand past the 200 byte limit \
                   \when decompressed"

    it "explains a docx that could not be decompressed without naming a file" $
      admissionMessage (CorruptDocxStream "premature end of compressed data stream")
        `shouldBe` "A .docx in this upload could not be decompressed: \
                   \premature end of compressed data stream"

  describe "admitDocxIO" $ do
    it "returns the measured size of a clean archive as a value" $
      admitDocxIO generous 0 "notes.docx" (zipOf [("word/document.xml", documentPart)])
        `shouldReturn` Right 13

    it "turns a corrupt deflate stream into a verdict, not an exception" $
      admitDocxIO generous 0 "notes.docx" truncatedStream
        `shouldReturn` Left (CorruptDocxStream "premature end of compressed data stream")
