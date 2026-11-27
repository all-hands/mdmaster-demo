module Mdmaster.Web.View.DiffSpec (spec) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime (..), fromGregorian)
import Lucid (Html, renderText)
import Mdmaster.App.Diff (DiffHunk (..), DiffKind (..), DiffLine (..))
import Mdmaster.App.Pipeline (diffContext)
import Mdmaster.App.Types
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionSchema (..), OptionValues, defaultValues)
import Mdmaster.Web.Api (batchFileDiffHref, batchFileHref)
import Mdmaster.Web.View.Diff
  (copyClass, copyHrefAttribute, copyLabel, diffTitleClass, diffView, wordChangedClass)
import Mdmaster.Web.View.DiffContext (contextActiveClass, diffContextCap, diffContextChoices)
import Test.Hspec

render :: Html () -> Text
render = TL.toStrict . renderText

hostilePath :: FilePath
hostilePath = "<script>alert(1)</script>.md"

hostileLine :: Text
hostileLine = "<img src=x onerror=alert(1)>"

changedFile :: BatchFile
changedFile = DocRow
  hostilePath
  Changed
  (Just hostileLine)
  (Just "before\n")
  (Just [DiffHunk 1 1 [DiffLine Added hostileLine]])

withHunks :: Maybe [DiffHunk] -> BatchFile -> BatchFile
withHunks hunks row = case row of
  DocRow p s r b _ ->
    DocRow p s r b hunks
  MediaRow {} -> row

failedFile :: BatchFile
failedFile = DocRow
  hostilePath
  (Failed "<script>alert(2)</script>")
  Nothing
  Nothing
  Nothing

unchangedFile :: BatchFile
unchangedFile = DocRow
  hostilePath
  Unchanged
  (Just hostileLine)
  (Just "before\n")
  (Just [])

mediaFile :: BatchFile
mediaFile = MediaRow
  "media/image1.png"
  "png"

noOptions :: OptionValues
noOptions = defaultValues (OptionSchema [])

ranAt :: UTCTime
ranAt = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

batch :: BatchResult
batch = BatchResult
  { brId        = BatchId "b1"
  , brAt        = ranAt
  , brOperation = OperationId "spec"
  , brOptions   = noOptions
  , brInput     = MarkdownInput
  , brFiles     = [changedFile, failedFile]
  }

renderedDiff :: Int -> BatchFile -> Text
renderedDiff ctx file = render (diffView (brId batch) 0 ctx file)

copyHref :: Text
copyHref = batchFileHref (brId batch) 0

spec :: Spec
spec = do
  describe "escaping" $ do
    it "escapes an uploaded file path in the diff heading" $
      renderedDiff diffContext changedFile `shouldSatisfy` (not . T.isInfixOf "<script>")

    it "escapes diff line content derived from the uploaded body" $ do
      let out = renderedDiff diffContext changedFile
      out `shouldSatisfy` (not . T.isInfixOf "<img")
      out `shouldSatisfy` T.isInfixOf "&lt;img"

    it "escapes a failure message" $
      renderedDiff diffContext failedFile `shouldSatisfy` (not . T.isInfixOf "<script>")

  describe "elided lines between hunks" $ do
    it "starts counting at the first line actually skipped" $ do
      let hunks =
            [ DiffHunk 1 1
                [DiffLine Context "a", DiffLine Added "NEW", DiffLine Context "b"]
            , DiffHunk 11 12 [DiffLine Context "k", DiffLine Removed "y"]
            ]
      renderedDiff diffContext (withHunks (Just hunks) changedFile)
        `shouldSatisfy` T.isInfixOf "lines 3–10 unchanged"

    it "says line, not lines, when exactly one was skipped" $ do
      let hunks =
            [ DiffHunk 1 1 [DiffLine Context "a"]
            , DiffHunk 3 3 [DiffLine Removed "c"]
            ]
      renderedDiff diffContext (withHunks (Just hunks) changedFile)
        `shouldSatisfy` T.isInfixOf "line 2 unchanged"

    it "shows no separator when the hunks are adjacent" $ do
      let hunks =
            [ DiffHunk 1 1 [DiffLine Context "a", DiffLine Added "NEW"]
            , DiffHunk 2 3 [DiffLine Removed "b"]
            ]
      renderedDiff diffContext (withHunks (Just hunks) changedFile)
        `shouldSatisfy` (not . T.isInfixOf "unchanged ")

  describe "word-level emphasis" $ do
    it "wraps the changed tokens in their own spans, both sides" $ do
      let hunks =
            [ DiffHunk 1 1
                [ DiffLine Removed "the quick fox"
                , DiffLine Added "the slow fox"
                ]
            ]
          out = renderedDiff diffContext (withHunks (Just hunks) changedFile)
          emphasised body =
            "<span class=\"" <> wordChangedClass <> "\">" <> body <> "</span>"
      out `shouldSatisfy` T.isInfixOf (emphasised "quick ")
      out `shouldSatisfy` T.isInfixOf (emphasised "slow ")
      out `shouldSatisfy` (not . T.isInfixOf (emphasised "the "))
      out `shouldSatisfy` (not . T.isInfixOf (emphasised "fox"))

    it "nests them inside the linetext cell rather than building strings" $
      renderedDiff diffContext changedFile
        `shouldSatisfy` T.isInfixOf ("linetext\"><span class=\"" <> wordChangedClass <> "\">")

    it "escapes a token it emphasises" $
      renderedDiff diffContext changedFile
        `shouldSatisfy` T.isInfixOf ("&lt;img src=x onerror=alert(1)&gt;</span>")

    it "leaves a context row without any emphasis" $ do
      let hunks = [DiffHunk 1 1 [DiffLine Context "plain"]]
      renderedDiff diffContext (withHunks (Just hunks) changedFile)
        `shouldSatisfy` (not . T.isInfixOf wordChangedClass)

  describe "the diff header" $
    it "totals the lines added and removed" $ do
      let out = renderedDiff diffContext changedFile
      out `shouldSatisfy` T.isInfixOf ">+1<"
      out `shouldSatisfy` T.isInfixOf ">−0<"

  describe "the context control" $ do
    let choiceHref value = batchFileDiffHref (brId batch) 0 (Just value)
        contextLabel value = fromMaybe "" (lookup value diffContextChoices)

    it "offers every choice as an htmx link back to this same route" $ do
      let out = renderedDiff 1 changedFile
      mapM_ (\(value, _) -> do
              out `shouldSatisfy` T.isInfixOf ("href=\"" <> choiceHref value <> "\"")
              out `shouldSatisfy` T.isInfixOf ("hx-get=\"" <> choiceHref value <> "\""))
            diffContextChoices
      out `shouldSatisfy` T.isInfixOf "hx-target=\"#diff-pane\""

    it "labels the choices the way the pane's own vocabulary names them" $ do
      let out = renderedDiff diffContext changedFile
      mapM_ (\(_, label) -> out `shouldSatisfy` T.isInfixOf (">" <> label <> "</"))
            diffContextChoices

    it "marks the pane's own context active, and only it" $ do
      let out = renderedDiff diffContext changedFile
          active = "<span class=\"diff-context-choice " <> contextActiveClass
          count = length (T.breakOnAll active out)
      count `shouldBe` 1
      out `shouldSatisfy` T.isInfixOf (active <> "\" aria-current=\"true\">" <> contextLabel diffContext <> "</span>")

    it "moves the mark when the pane is rendered at another choice" $ do
      renderedDiff 0 changedFile
        `shouldSatisfy` T.isInfixOf ("aria-current=\"true\">" <> contextLabel 0 <> "</span>")
      renderedDiff diffContextCap changedFile
        `shouldSatisfy` T.isInfixOf ("aria-current=\"true\">" <> contextLabel diffContextCap <> "</span>")

    it "is not rendered where no diff shows" $
      renderedDiff diffContext failedFile `shouldSatisfy` (not . T.isInfixOf "diff-context")

  describe "the copy button" $
    let theButton = renderedDiff diffContext changedFile in do
      it "reaches a changed file's pane as a button naming its download" $ do
        theButton `shouldSatisfy`
          T.isInfixOf ("<button type=\"button\" class=\"" <> copyClass <> "\"")
        theButton `shouldSatisfy`
          T.isInfixOf (copyHrefAttribute <> "=\"" <> copyHref <> "\"")
        theButton `shouldSatisfy` T.isInfixOf (">" <> copyLabel <> "</button>")

      it "reaches an unchanged file's pane, whose download is the same text" $
        renderedDiff diffContext (unchangedFile)
          `shouldSatisfy` T.isInfixOf copyClass

      it "is withheld from a failure's pane, which has nothing to copy" $
        renderedDiff diffContext failedFile `shouldSatisfy` (not . T.isInfixOf copyClass)

      it "is withheld from a media row, whose download is not text" $
        renderedDiff diffContext mediaFile `shouldSatisfy` (not . T.isInfixOf copyClass)

  describe "a media row" $
    it "renders the unchanged note and no diff table" $
      renderedDiff diffContext mediaFile `shouldSatisfy` \html ->
        T.isInfixOf "class=\"unchanged-note\"" html
          && not (T.isInfixOf "diff-table" html)

  describe "the pane heading" $
    let theHeading = renderedDiff diffContext changedFile in do
      it "carries the class app.js focuses a swap by" $
        theHeading `shouldSatisfy` T.isInfixOf ("class=\"" <> diffTitleClass <> "\"")

      it "is focusable by the client, and out of the tab ring itself" $
        theHeading `shouldSatisfy` T.isInfixOf
          ("<h2 class=\"" <> diffTitleClass <> "\" tabindex=\"-1\">")

      it "reaches a failure's pane too, which is where its reason is read" $
        renderedDiff diffContext failedFile
          `shouldSatisfy` T.isInfixOf ("class=\"" <> diffTitleClass <> "\"")
