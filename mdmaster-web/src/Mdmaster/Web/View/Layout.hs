module Mdmaster.Web.View.Layout
  ( page
  , dropZone
  , acceptedExtensions
  , expansionNote
  , dropOverlay
  , dropOverlayId
  , dropZoneId
  , filePickerId
  , folderPickerId
  , fileSummaryId
  , fileCountId
  , clearFilesId
  , acceptedFilesId
  , maxFilesAttribute
  , maxFileBytesAttribute
  , acceptExtensionsAttribute
  , expansionNoteAttribute
  , acceptListClass
  , expansionNoteClass
  , formPanel
  , humanBytes
  , humanDuration
  , privacyNote
  , sourceLabel
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Mdmaster.App.Docx.Admission (DocxLimits (..))
import Mdmaster.Core.Operation (InputFormat (..), inputExtensions)
import Mdmaster.Web.Api (staticHref)
import Mdmaster.Web.Config (Config (..), sweepIntervalSeconds)
import Mdmaster.Web.Limits (Limits (..), acceptedPhrase)
import Mdmaster.Web.View.Demo (demoNotice)

page :: Config -> Text -> Html () -> Html ()
page config title body = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [name_ "color-scheme", content_ "light dark"]
      meta_ [ name_ "description"
            , content_ "Operations on Markdown files, in the browser." ]
      title_ (toHtml title)
      link_ [rel_ "icon", type_ "image/svg+xml", href_ (staticHref "favicon.svg")]
      link_ [rel_ "stylesheet", href_ (staticHref "app.css")]
      meta_ [ name_ "htmx-config"
            , content_ "{\"includeIndicatorStyles\":false}" ]
      script_ [ src_ (staticHref "htmx.min.js"), defer_ "defer"
              , integrity_ "sha384-2OatzQy1H+Zd/IIrjr1TcuDGqLXeHhbooAyJY1KdQMKnr4LZ22k31GBLdYKHmVjg" ] ("" :: Text)
      script_ [src_ (staticHref "app.js"), defer_ "defer"] ("" :: Text)
    body_ $ do
      header_ [class_ "site-header"] $ do
        h1_ [class_ "brand"] $ a_ [href_ "/", class_ "brand-link"] $ do
          span_ [class_ "brand-mark", term "aria-hidden" "true"] "📝"
          span_ [class_ "brand-name"] "mdmaster"
          span_ [class_ "brand-tagline"] "tidy Markdown, in your browser"
        mapM_ (const demoNotice) (cfgDemo config)
      main_ body
      footer_ [class_ "site-footer"] $ do
        p_ (toHtml (privacyNote config))
        p_ $ a_ [href_ (cfgSourceUrl config)] (toHtml sourceLabel)

privacyNote :: Config -> Text
privacyNote config =
  "Anonymous and stateless. The app processes files in memory and writes none \
  \to disk (a reverse proxy in front may buffer uploads to temporary files). \
  \Results can be downloaded for up to "
    <> humanDuration (round (cfgResultTtl config))
    <> " (sooner when the server is busy, so save what you need), and are \
       \removed from the result store within "
    <> humanDuration sweepIntervalSeconds <> " after that."

humanDuration :: Int -> Text
humanDuration total = case parts of
  []  -> "0 seconds"
  [p] -> p
  _   -> T.intercalate ", " (init parts) <> " and " <> last parts
  where
    (hours, rest)      = total `divMod` 3600
    (minutes, seconds) = rest `divMod` 60
    parts = [ tshow n <> " " <> unit <> (if n == 1 then "" else "s")
            | (n, unit) <- [(hours, "hour"), (minutes, "minute"), (seconds, "second")]
            , n > 0 ]

sourceLabel :: Text
sourceLabel = "Source (GPL-2.0-or-later)"

acceptedExtensions :: InputFormat -> Text
acceptedExtensions = T.intercalate "," . map T.pack . inputExtensions

expansionNote :: Limits -> InputFormat -> Maybe Text
expansionNote limits format = case format of
  MarkdownInput -> Nothing
  DocxInput     -> Just $
    "Unpacked, each may be up to " <> humanBytes (dlFileBytes docx)
      <> ", and the batch up to " <> humanBytes (dlBatchBytes docx) <> " in all."
  where
    docx = limDocx limits

dropZone :: Limits -> InputFormat -> Html ()
dropZone limits format = section_
  [ id_ dropZoneId
  , class_ "card drop-zone"
  , term maxFilesAttribute (tshow (limMaxFiles limits))
  , term maxFileBytesAttribute (tshow (limMaxFileBytes limits))
  , term acceptExtensionsAttribute accepted
  ] $ do
    p_ [class_ "drop-mark", term "aria-hidden" "true"] "🗂️"
    p_ [class_ "drop-title"] "Drop files or a folder here"
    p_ [class_ "drop-hint"] $ do
      "Only "
      span_ [class_ acceptListClass] (toHtml (acceptedPhrase format))
      toHtml $ " files are uploaded — up to "
        <> tshow (limMaxFiles limits) <> " of them, "
        <> humanBytes (limMaxFileBytes limits) <> " each."
    p_ ([class_ ("drop-hint " <> expansionNoteClass)] <> [hidden_ "hidden" | null note])
      (toHtml (fromMaybe "" note))

    div_ [class_ "pickers"] $ do
      input_ [ type_ "file", id_ filePickerId, class_ "visually-hidden"
             , multiple_ "multiple", accept_ accepted ]
      label_ [class_ "picker", for_ filePickerId] "Choose files"
      input_ [ type_ "file", id_ folderPickerId, class_ "visually-hidden"
             , term "webkitdirectory" "true", multiple_ "multiple" ]
      label_ [class_ "picker", for_ folderPickerId] "Choose a folder"

    div_ [id_ fileSummaryId, class_ "file-summary", hidden_ "hidden"] $ do
      p_ [id_ fileCountId, class_ "file-count", term "aria-live" "polite"] mempty
      button_ [type_ "button", id_ clearFilesId, class_ "link-button"] "Clear all"

    ul_ [id_ acceptedFilesId, class_ "accepted-files"] mempty
  where
    accepted = acceptedExtensions format
    note = expansionNote limits format

dropOverlay :: Html ()
dropOverlay = div_ [id_ dropOverlayId, class_ "drop-overlay", term "aria-hidden" "true"] $
  p_ [class_ "drop-overlay-card"] "Drop to add files"

dropOverlayId :: Text
dropOverlayId = "drop-overlay"

dropZoneId :: Text
dropZoneId = "drop-zone"

filePickerId :: Text
filePickerId = "file-picker"

folderPickerId :: Text
folderPickerId = "folder-picker"

fileSummaryId :: Text
fileSummaryId = "file-summary"

fileCountId :: Text
fileCountId = "file-count"

clearFilesId :: Text
clearFilesId = "clear-files"

acceptedFilesId :: Text
acceptedFilesId = "accepted-files"

maxFilesAttribute :: Text
maxFilesAttribute = "data-max-files"

maxFileBytesAttribute :: Text
maxFileBytesAttribute = "data-max-file-bytes"

acceptExtensionsAttribute :: Text
acceptExtensionsAttribute = "data-accept-extensions"

expansionNoteAttribute :: Text
expansionNoteAttribute = "data-expansion-note"

acceptListClass :: Text
acceptListClass = "accept-list"

expansionNoteClass :: Text
expansionNoteClass = "expansion-note"

formPanel :: Html () -> Html ()
formPanel = div_ [id_ "form-panel", class_ "card form-panel"]

humanBytes :: Int -> Text
humanBytes n
  | n >= gb   = scaled gb "GiB"
  | n >= mb   = scaled mb "MiB"
  | n >= kb   = scaled kb "KiB"
  | n == 1    = "1 byte"
  | otherwise = tshow n <> " bytes"
  where
    kb = 1024
    mb = 1024 * kb
    gb = 1024 * mb
    scaled divisor unit =
      let whole = n `div` divisor
          tenth = (n * 10 `div` divisor) `mod` 10
       in if tenth == 0
            then tshow whole <> " " <> unit
            else tshow whole <> "." <> tshow tenth <> " " <> unit

tshow :: Int -> Text
tshow = T.pack . show
