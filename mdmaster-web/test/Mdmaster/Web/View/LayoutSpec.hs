module Mdmaster.Web.View.LayoutSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Lucid (Html, renderText)
import Mdmaster.App.Docx.Admission (DocxLimits (..))
import Mdmaster.Core.Operation (InputFormat (..))
import Mdmaster.Web.Config (Config (..), defaultConfig, sweepIntervalSeconds)
import Mdmaster.Web.Limits (Limits (..), limitsFromConfig)
import Mdmaster.Web.View.Layout
import Test.Hspec

render :: Html () -> Text
render = TL.toStrict . renderText

limits :: Limits
limits = limitsFromConfig defaultConfig

appJs :: IO Text
appJs =
  T.replace "mdmaster" "" . T.toLower . TE.decodeUtf8 <$> BS.readFile "static/app.js"

spec :: Spec
spec = do
  describe "the page shell" $ do
    it "declares a language, so a screen reader knows how to pronounce it" $
      render (page defaultConfig "mdmaster" mempty) `shouldSatisfy` T.isInfixOf "<html lang=\"en\">"

    it "points at a favicon, so a visit is not one 404 heavier" $
      render (page defaultConfig "mdmaster" mempty)
        `shouldSatisfy` T.isInfixOf "/static/favicon.svg"

    it "tells the browser both colour schemes are handled" $
      render (page defaultConfig "mdmaster" mempty) `shouldSatisfy` T.isInfixOf "light dark"

    it "escapes the title rather than emitting markup" $
      render (page defaultConfig "<script>alert(1)</script>" mempty)
        `shouldSatisfy` (not . T.isInfixOf "<script>")

  describe "the footer" $ do
    it "offers the source, labelled with the licence that asks for the offer" $ do
      let out = render (page defaultConfig "mdmaster" mempty)
      out `shouldSatisfy` T.isInfixOf "https://github.com/all-hands/mdmaster-demo"
      out `shouldSatisfy` T.isInfixOf ("href=\"" <> cfgSourceUrl defaultConfig <> "\"")
      out `shouldSatisfy` T.isInfixOf "Source (GPL-2.0-or-later)"

    it "points the source offer at the configured URL" $ do
      let configured = defaultConfig { cfgSourceUrl = "https://example.org/mdmaster-src.tar.gz" }
          out = render (page configured "mdmaster" mempty)
      out `shouldSatisfy` T.isInfixOf "href=\"https://example.org/mdmaster-src.tar.gz\""
      out `shouldSatisfy` (not . T.isInfixOf "https://github.com/all-hands/mdmaster-demo")
      out `shouldSatisfy` T.isInfixOf "Source (GPL-2.0-or-later)"

    it "states the retention the default configuration ships" $ do
      let minutes = round (cfgResultTtl defaultConfig) `div` 60 :: Int
      render (page defaultConfig "mdmaster" mempty)
        `shouldSatisfy` T.isInfixOf
          ("downloaded for up to " <> T.pack (show minutes) <> " minutes")
      render (page defaultConfig "mdmaster" mempty)
        `shouldSatisfy` T.isInfixOf "writes none to disk (a reverse proxy in front may buffer uploads"

    it "states the configured retention, not the default's" $ do
      let out ttl = render (page defaultConfig { cfgResultTtl = ttl } "mdmaster" mempty)
      out 7200 `shouldSatisfy` T.isInfixOf "downloaded for up to 2 hours ("
      out 90 `shouldSatisfy` T.isInfixOf "downloaded for up to 1 minute and 30 seconds ("
      out 45 `shouldSatisfy` T.isInfixOf "downloaded for up to 45 seconds ("
      out 7200 `shouldSatisfy` (not . T.isInfixOf "30 minutes")

    it "presents the retention as a maximum" $
      render (page defaultConfig "mdmaster" mempty)
        `shouldSatisfy` T.isInfixOf "sooner when the server is busy, so save what you need"

    it "says the results stay in the store until the next sweep, and no longer" $
      render (page defaultConfig "mdmaster" mempty)
        `shouldSatisfy` T.isInfixOf
          ("removed from the result store within " <> humanDuration sweepIntervalSeconds <> " after that")

    it "ships no inline style and no inline script, what the CSP assumes" $ do
      let out = render (page defaultConfig "mdmaster" mempty)
      out `shouldSatisfy` (not . T.isInfixOf "style=")
      out `shouldSatisfy` (not . T.isInfixOf "<style")
      out `shouldSatisfy` (not . T.isInfixOf "<script>")

  describe "the drop zone" $ do
    it "keeps the ids app.js binds to" $ do
      let out = render (dropZone limits MarkdownInput)
      mapM_ (\i -> out `shouldSatisfy` T.isInfixOf ("id=\"" <> i <> "\""))
        ["drop-zone", "file-picker", "folder-picker", "accepted-files", "file-count"]

    it "states the configured limits instead of letting the server surprise you" $ do
      let out = render (dropZone limits MarkdownInput)
      out `shouldSatisfy` T.isInfixOf "500"
      out `shouldSatisfy` T.isInfixOf "10 MiB"

    it "publishes the limits for the client to check against" $ do
      let out = render (dropZone limits MarkdownInput)
      out `shouldSatisfy` T.isInfixOf "data-max-files=\"500\""
      out `shouldSatisfy` T.isInfixOf "data-max-file-bytes=\"10485760\""

    it "labels both file inputs" $ do
      let out = render (dropZone limits MarkdownInput)
      out `shouldSatisfy` T.isInfixOf "for=\"file-picker\""
      out `shouldSatisfy` T.isInfixOf "for=\"folder-picker\""

  describe "the expansion note" $ do
    it "states both expansion caps when the operation reads .docx" $ do
      let docx = limDocx limits
          out = render (dropZone limits DocxInput)
      out `shouldSatisfy` T.isInfixOf (humanBytes (dlFileBytes docx))
      out `shouldSatisfy` T.isInfixOf (humanBytes (dlBatchBytes docx))
      out `shouldSatisfy` T.isInfixOf "50 MiB"
      out `shouldSatisfy` T.isInfixOf "200 MiB"

    it "follows a configured cap rather than the default" $ do
      let custom = limits { limDocx = DocxLimits { dlFileBytes = 2097152, dlBatchBytes = 5242880 } }
          out = render (dropZone custom DocxInput)
      out `shouldSatisfy` T.isInfixOf "2 MiB"
      out `shouldSatisfy` T.isInfixOf "5 MiB"
      out `shouldSatisfy` (not . T.isInfixOf "50 MiB")

    it "is rendered visible for .docx, hidden for Markdown" $ do
      let visible = "class=\"drop-hint expansion-note\">"
          hiddenP = "class=\"drop-hint expansion-note\" hidden=\"hidden\">"
      render (dropZone limits DocxInput) `shouldSatisfy` T.isInfixOf visible
      render (dropZone limits MarkdownInput) `shouldSatisfy` T.isInfixOf hiddenP
      render (dropZone limits MarkdownInput) `shouldSatisfy` (not . T.isInfixOf "200 MiB")

    it "is the one sentence the zone renders" $
      case expansionNote limits DocxInput of
        Nothing   -> expectationFailure "a .docx format has an expansion note"
        Just note -> render (dropZone limits DocxInput) `shouldSatisfy` T.isInfixOf note

  describe "the drop-anywhere overlay" $ do
    it "carries the id app.js reveals it by, and the sentence it never spells" $ do
      let out = render dropOverlay
      out `shouldSatisfy` T.isInfixOf ("id=\"" <> dropOverlayId <> "\"")
      out `shouldSatisfy` T.isInfixOf "Drop to add files"

    it "is aria-hidden, so the start of a drag is not an announcement" $
      render dropOverlay `shouldSatisfy` T.isInfixOf
        ("id=\"" <> dropOverlayId
          <> "\" class=\"drop-overlay\" aria-hidden=\"true\"")

  describe "the accepted extensions" $ do
    it "puts them on the file picker, so the OS dialog filters for the visitor" $ do
      render (dropZone limits MarkdownInput)
        `shouldSatisfy` T.isInfixOf "accept=\".md,.markdown\""
      render (dropZone limits DocxInput) `shouldSatisfy` T.isInfixOf "accept=\".docx\""

    it "publishes them beside the limits, for the filter that actually runs" $ do
      render (dropZone limits MarkdownInput)
        `shouldSatisfy` T.isInfixOf "data-accept-extensions=\".md,.markdown\""
      render (dropZone limits DocxInput)
        `shouldSatisfy` T.isInfixOf "data-accept-extensions=\".docx\""

    it "names them in the hint, so the rule is visible without a failed upload" $ do
      render (dropZone limits MarkdownInput)
        `shouldSatisfy` T.isInfixOf ">.md and .markdown</span> files are uploaded"
      render (dropZone limits DocxInput)
        `shouldSatisfy` T.isInfixOf ">.docx</span> files are uploaded"

    it "names no format it does not accept" $ do
      let out = render (dropZone limits DocxInput)
      out `shouldSatisfy` (not . T.isInfixOf "Markdown")
      out `shouldSatisfy` (not . T.isInfixOf ".md")

    it "wraps the list in the element app.js rewrites when the operation changes" $
      render (dropZone limits MarkdownInput)
        `shouldSatisfy` T.isInfixOf "class=\"accept-list\""

  describe "the contract with static/app.js" $ do
    it "spells the published attribute the same way the drop zone does" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "dataset.acceptextensions"

    it "mirrors the option's expansion note into the element the zone renders" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "dataset.expansionnote"
      source `shouldSatisfy` T.isInfixOf "\".expansion-note\""
      render (dropZone limits DocxInput)
        `shouldSatisfy` T.isInfixOf "expansion-note"

    it "names no file format of its own" $ do
      source <- appJs
      mapM_ (\needle -> source `shouldSatisfy` (not . T.isInfixOf needle))
        ["markdown", ".md", ".docx"]

    it "posts the run where the form's action points, never at a literal" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "fetch(form.getattribute(\"action\")"
      source `shouldSatisfy` (not . T.isInfixOf "fetch(\"/")

    it "reveals the page-wide overlay through the body-level dragging state" $ do
      source <- appJs
      source `shouldSatisfy`
        T.isInfixOf ("getelementbyid(\"" <> T.toLower dropOverlayId <> "\")")
      source `shouldSatisfy` T.isInfixOf "body.classlist.add(\"dragging\")"
      source `shouldSatisfy` T.isInfixOf "body.classlist.remove(\"dragging\")"

    it "leaves a drop that landed on the zone to the zone's own listener" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "closest(\"#drop-zone\")"

    it "steps only through button rows, opening each by its own click" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "queryselector(\"button.file-name\")"
      source `shouldSatisfy` T.isInfixOf "button.click()"

    it "leaves fields and chords their keystrokes" $ do
      source <- appJs
      mapM_ (\tag -> source `shouldSatisfy`
              T.isInfixOf ("focused.tagname === \"" <> tag <> "\""))
        ["input", "textarea", "select"]
      source `shouldSatisfy` T.isInfixOf "event.altkey"
      source `shouldSatisfy` T.isInfixOf "event.shiftkey"

    it "focuses the pane's heading after a swap" $ do
      source <- appJs
      source `shouldSatisfy` T.isInfixOf "queryselector(\".diff-title\")"
      source `shouldSatisfy` T.isInfixOf "focus({ preventscroll: true })"

  describe "the form panel" $
    it "carries the id a rejected run replaces" $
      render (formPanel mempty) `shouldSatisfy` T.isInfixOf "id=\"form-panel\""

  describe "humanBytes" $ do
    it "drops a trailing zero" $ do
      humanBytes (10 * 1024 * 1024) `shouldBe` "10 MiB"
      humanBytes 1024 `shouldBe` "1 KiB"

    it "keeps one decimal when there is one" $
      humanBytes (1024 * 1024 + 512 * 1024) `shouldBe` "1.5 MiB"

    it "counts small files in bytes" $ do
      humanBytes 0 `shouldBe` "0 bytes"
      humanBytes 1 `shouldBe` "1 byte"
      humanBytes 999 `shouldBe` "999 bytes"

    it "reaches gigabytes" $
      humanBytes (3 * 1024 * 1024 * 1024) `shouldBe` "3 GiB"

  describe "humanDuration" $ do
    it "says a round count in its largest unit" $ do
      humanDuration 1800 `shouldBe` "30 minutes"
      humanDuration 3600 `shouldBe` "1 hour"
      humanDuration 60 `shouldBe` "1 minute"

    it "spells out what is not a round count, leaving zero units out" $ do
      humanDuration 90 `shouldBe` "1 minute and 30 seconds"
      humanDuration 3661 `shouldBe` "1 hour, 1 minute and 1 second"
      humanDuration 3605 `shouldBe` "1 hour and 5 seconds"
      humanDuration 5400 `shouldBe` "1 hour and 30 minutes"
      humanDuration 1 `shouldBe` "1 second"
