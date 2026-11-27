module Mdmaster.Web.View.ResultsSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Lucid (Html, renderText)
import Mdmaster.App.Diff (DiffHunk (..), DiffKind (..), DiffLine (..))
import Mdmaster.App.Types
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionSchema (..), OptionValues, defaultValues)
import Mdmaster.Web.Api (batchFileDiffHref, batchFileHref)
import Mdmaster.Web.View.Results
  ( downloadAllLabel
  , emptyResults
  , errorBanner
  , errorBannerTemplate
  , errorBannerTemplateId
  , expiryAttribute
  , expirySentence
  , fileFilterInputId
  , fileFilterPlaceholder
  , fileFilterThreshold
  , fileListView
  , plural
  )
import Test.Hspec

render :: Html () -> Text
render = TL.toStrict . renderText

hostilePath :: FilePath
hostilePath = "<script>alert(1)</script>.md"

hostileLine :: Text
hostileLine = "<img src=x onerror=alert(1)>"

changedFile :: BatchFile
changedFile = changedAt hostilePath

changedAt :: FilePath -> BatchFile
changedAt path = DocRow
  path
  Changed
  (Just hostileLine)
  (Just "before\n")
  (Just [DiffHunk 1 1 [DiffLine Added hostileLine]])

withHunks :: Maybe [DiffHunk] -> BatchFile -> BatchFile
withHunks hunks row = case row of
  DocRow p s r b _ ->
    DocRow p s r b hunks
  MediaRow {} -> row

unchangedFile :: BatchFile
unchangedFile = DocRow
  hostilePath
  Unchanged
  (Just hostileLine)
  (Just "before\n")
  (Just [])

failedFile :: BatchFile
failedFile = DocRow
  hostilePath
  (Failed "<script>alert(2)</script>")
  Nothing
  Nothing
  Nothing

noOptions :: OptionValues
noOptions = defaultValues (OptionSchema [])

ranAt :: UTCTime
ranAt = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

expiresAt :: UTCTime
expiresAt = addUTCTime 1800 ranAt

list :: BatchResult -> Text
list b = render (fileListView ranAt expiresAt b)

batch :: BatchResult
batch = BatchResult
  { brId        = BatchId "b1"
  , brAt        = ranAt
  , brOperation = OperationId "spec"
  , brOptions   = noOptions
  , brInput     = MarkdownInput
  , brFiles     = [changedFile, failedFile]
  }

convertedFile :: BatchFile
convertedFile = DocRow
  "notes.md"
  Changed
  (Just "# Notes\n")
  Nothing
  Nothing

mediaFile :: BatchFile
mediaFile = MediaRow
  "media/image1.png"
  "png"

docxBatch :: BatchResult
docxBatch = BatchResult
  { brId        = BatchId "b2"
  , brAt        = ranAt
  , brOperation = OperationId "docx"
  , brOptions   = noOptions
  , brInput     = DocxInput
  , brFiles     = [convertedFile]
  }

batchOf :: Int -> BatchResult
batchOf n = batch
  { brFiles =
      [ changedAt ("chapter-" <> show i <> ".md")
      | i <- [0 .. n - 1]
      ]
  }

spec :: Spec
spec = do
  describe "escaping" $ do
    it "escapes an uploaded file path in the file list" $ do
      let out = list (batch)
      out `shouldSatisfy` (not . T.isInfixOf "<script>")
      out `shouldSatisfy` T.isInfixOf "&lt;script&gt;"

    it "escapes an error banner message" $
      render (errorBanner "<script>alert(3)</script>")
        `shouldSatisfy` (not . T.isInfixOf "<script>")

  describe "status badges" $ do
    it "labels a changed file" $
      list (batch) `shouldSatisfy` T.isInfixOf "status-changed"

    it "labels a failed file as an error" $ do
      let out = list (batch)
      out `shouldSatisfy` T.isInfixOf "status-failed"
      out `shouldSatisfy` T.isInfixOf ">error<"

  describe "row counts" $ do
    it "counts a changed file's added and removed lines beside its badge" $ do
      let hunks =
            [ DiffHunk 1 1
                [ DiffLine Context "a"
                , DiffLine Added "x"
                , DiffLine Added "y"
                , DiffLine Removed "z"
                ]
            ]
          out = list (batch { brFiles = [(withHunks (Just hunks) changedFile)] })
      out `shouldSatisfy` T.isInfixOf ">+2<"
      out `shouldSatisfy` T.isInfixOf ">−1<"

    it "dresses them in the stat classes the pane's header wears" $ do
      let out = list (batch)
      out `shouldSatisfy` T.isInfixOf "class=\"file-stat\""
      out `shouldSatisfy` T.isInfixOf "stat-added"
      out `shouldSatisfy` T.isInfixOf "stat-removed"

    it "shows none on a converted row, which has no before-state to count" $
      list (docxBatch) `shouldSatisfy` (not . T.isInfixOf "file-stat")

    it "shows none on an unchanged media row, which has no diff at all" $
      list (docxBatch { brFiles = [convertedFile, mediaFile] })
        `shouldSatisfy` (not . T.isInfixOf "file-stat")

    it "shows none on an unchanged Markdown row, whose diff came back empty" $
      list (batch
                { brFiles = [unchangedFile] })
        `shouldSatisfy` (not . T.isInfixOf "file-stat")

    it "shows none on a failed row, which has nothing to count" $
      list (batch { brFiles = [failedFile] })
        `shouldSatisfy` (not . T.isInfixOf "file-stat")

  describe "the summary" $ do
    it "counts in words a person would use" $ do
      let out = list (batch)
      out `shouldSatisfy` T.isInfixOf "1 changed file"
      out `shouldSatisfy` (not . T.isInfixOf "file(s)")

    it "leaves out a count of zero rather than reporting it" $
      list (batch) `shouldSatisfy` (not . T.isInfixOf "unchanged file")

    it "agrees with itself about one" $ do
      plural 1 "file" `shouldBe` "1 file"
      plural 0 "file" `shouldBe` "0 files"
      plural 2 "file" `shouldBe` "2 files"

  describe "the zip link" $ do
    it "is offered when there is at least one file to pack" $
      list (batch) `shouldSatisfy` T.isInfixOf downloadAllLabel

    it "is withheld when every file failed and the archive would be empty" $
      list (batch { brFiles = [failedFile] })
        `shouldSatisfy` (not . T.isInfixOf downloadAllLabel)

  describe "the file filter" $ do
    it "renders above the list once the batch passes the threshold" $ do
      let out = list (batchOf (fileFilterThreshold + 1))
      out `shouldSatisfy` T.isInfixOf ("id=\"" <> fileFilterInputId <> "\"")
      out `shouldSatisfy` T.isInfixOf "type=\"search\""
      out `shouldSatisfy` T.isInfixOf ("placeholder=\"" <> fileFilterPlaceholder <> "\"")
      out `shouldSatisfy` T.isInfixOf ("<label for=\"" <> fileFilterInputId <> "\"")
      out `shouldSatisfy` T.isInfixOf "file-filter-count"
      out `shouldSatisfy` T.isInfixOf "file-filter-nomatch"

    it "renders none of itself at the threshold, or below it" $ do
      let at = list (batchOf fileFilterThreshold)
          below = list (batchOf (fileFilterThreshold - 1))
      mapM_ (\out -> do
              out `shouldSatisfy` (not . T.isInfixOf fileFilterInputId)
              out `shouldSatisfy` (not . T.isInfixOf fileFilterPlaceholder))
            [at, below]

    it "carries each row's path for the client to match against" $ do
      let out = list (batch)
      out `shouldSatisfy` T.isInfixOf "data-path="
      out `shouldSatisfy` T.isInfixOf "data-path=\"&lt;script&gt;"

  describe "the diff affordance" $ do
    it "is offered for a Markdown batch, which has a before-state" $
      list (batch) `shouldSatisfy` T.isInfixOf (batchFileDiffHref (brId batch) 0 Nothing)

    it "is withheld for a docx batch, and so is the pane it would target" $ do
      let out = list (docxBatch)
      out `shouldSatisfy` (not . T.isInfixOf "/diff")
      out `shouldSatisfy` (not . T.isInfixOf "id=\"diff-pane\"")

    it "leaves the file name in the slot the button held" $
      list (docxBatch)
        `shouldSatisfy` T.isInfixOf "<span class=\"file-name\""

    it "gives back the column the pane occupied instead of leaving it empty" $ do
      list (docxBatch) `shouldSatisfy` T.isInfixOf "results-body no-diff"
      list (batch) `shouldSatisfy` (not . T.isInfixOf "no-diff")

    it "still offers the download, which is the whole point of the run" $
      list (docxBatch) `shouldSatisfy` T.isInfixOf (batchFileHref (brId docxBatch) 0)

    it "keeps it for a failed docx file, whose pane carries the reason" $ do
      let out = list (docxBatch { brFiles = [convertedFile, failedFile] })
      out `shouldSatisfy` (not . T.isInfixOf (batchFileDiffHref (brId docxBatch) 0 Nothing))
      out `shouldSatisfy` T.isInfixOf (batchFileDiffHref (brId docxBatch) 1 Nothing)
      out `shouldSatisfy` T.isInfixOf "id=\"diff-pane\""

    it "promises the pane only what that batch can put in it" $ do
      list (batch) `shouldSatisfy` T.isInfixOf "what changed"
      list (docxBatch { brFiles = [convertedFile, failedFile] })
        `shouldSatisfy` T.isInfixOf "why it failed"

  describe "a media row" $ do
    it "is listed with its media path and offers the download" $
      list (docxBatch { brFiles = [convertedFile, mediaFile] })
        `shouldSatisfy` T.isInfixOf (batchFileHref (brId docxBatch) 1)

    it "wears the unchanged badge and opens no diff" $ do
      let out = list (docxBatch { brFiles = [convertedFile, mediaFile] })
      out `shouldSatisfy` T.isInfixOf "media/image1.png"
      out `shouldSatisfy` T.isInfixOf ">unchanged<"
      out `shouldSatisfy` (not . T.isInfixOf (batchFileDiffHref (brId docxBatch) 1 Nothing))

    it "gives back the pane when media is all a batch has to show" $ do
      let out = list (docxBatch { brFiles = [mediaFile] })
      out `shouldSatisfy` T.isInfixOf "id=\"results\""
      out `shouldSatisfy` T.isInfixOf "results-body no-diff"
      out `shouldSatisfy` T.isInfixOf downloadAllLabel

  describe "the idle container" $ do
    it "carries the id every response replaces" $
      render emptyResults `shouldSatisfy` T.isInfixOf "id=\"results\""

    it "carries no expiry to count down" $
      render emptyResults `shouldSatisfy` (not . T.isInfixOf expiryAttribute)

  describe "the expiry stamp" $ do
    it "publishes the absolute expiry as a data attribute" $ do
      let out = list (batch)
      out `shouldSatisfy` T.isInfixOf (expiryAttribute <> "=\"2026-11-06T10:30:00Z\"")

    it "opens on the minutes the store still has left" $
      list (batch) `shouldSatisfy` T.isInfixOf (expirySentence 30)

    it "counts one minute in the singular" $
      expirySentence 1 `shouldBe` "Available for download for up to 1 more minute (sooner if the server needs the room, so save what you need now)."

    it "promises nothing once the store's zero has passed" $
      render (fileListView (addUTCTime 1 expiresAt) expiresAt batch)
        `shouldSatisfy` (not . T.isInfixOf "more minutes")

  describe "the banner template" $
    it "ships the banner inert, with a span the client can fill" $ do
      let out = render errorBannerTemplate
      out `shouldSatisfy` T.isInfixOf ("id=\"" <> errorBannerTemplateId <> "\"")
      out `shouldSatisfy` T.isInfixOf "banner-message"
