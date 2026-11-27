module Mdmaster.Web.View.Results
  ( fileListView
  , emptyResults
  , expiredView
  , expiredHeadline
  , nothingToZipView
  , nothingToZipHeadline
  , downloadAllLabel
  , expiryAttribute
  , expirySentence
  , fileFilterThreshold
  , fileFilterInputId
  , fileFilterPlaceholder
  , errorBanner
  , errorBannerTemplate
  , errorBannerTemplateId
  , plural
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime)
import Lucid
import Mdmaster.App.Types
import Mdmaster.App.Zip (zipEntryPaths)
import Mdmaster.App.Zip.Manifest (utcStamp)
import Mdmaster.Core.Operation (InputFormat (..))
import Mdmaster.Web.Api (batchFileDiffHref, batchFileHref, batchZipHref)

emptyResults :: Html ()
emptyResults = div_ [id_ "results", class_ "results results-idle"] $
  p_ [class_ "results-placeholder"] $ do
    span_ [class_ "placeholder-mark", term "aria-hidden" "true"] "🪄"
    span_ "Your preview will appear here."

fileListView :: UTCTime -> UTCTime -> BatchResult -> Html ()
fileListView now expiry batch =
  div_ [id_ "results", class_ "results", term expiryAttribute (utcStamp expiry)] $ do
    div_ [class_ "results-head"] $ do
      h2_ [class_ "results-title"] $ do
        span_ [class_ "results-mark", term "aria-hidden" "true"]
              (toHtml (headlineMark batch))
        toHtml (headline batch)
      ul_ [class_ "pills"] (mapM_ pill (countPills batch))
      if null (zipEntryPaths batch)
        then mempty
        else a_ [ href_ (batchZipHref (brId batch))
                , class_ "button download-all"
                , download_ ""
                ]
                (toHtml downloadAllLabel)

    expiryLine now expiry

    if batchFailedCount batch > 0
      then p_ [class_ "summary-note"] $ toHtml $
             plural (batchFailedCount batch) "file" <> " failed and "
               <> (if batchFailedCount batch == 1 then "is" else "are")
               <> " left out of the zip."
      else mempty

    div_ [class_ (if anyOpens then "results-body" else "results-body no-diff")] $ do
      if length (brFiles batch) > fileFilterThreshold
        then div_ [class_ "file-list-side"] $ do
               fileFilter
               fileList
        else fileList
      if anyOpens
        then diffPane $ p_ [class_ "diff-placeholder"] $ do
               span_ [class_ "placeholder-mark", term "aria-hidden" "true"] "👀"
               span_ (toHtml placeholder)
        else mempty
  where
    anyOpens = any opensDiff (brFiles batch)
    fileList = ul_ [class_ "file-list"] $
      mapM_ (fileRow (brId batch)) (zip [0 :: Int ..] (brFiles batch))
    placeholder :: Text
    placeholder = case brInput batch of
      MarkdownInput -> "Pick a file to see what changed."
      _             -> "Pick a file to see why it failed."

expiryAttribute :: Text
expiryAttribute = "data-expires-at"

expirySentence :: Int -> Text
expirySentence minutes =
  "Available for download for up to " <> plural minutes "more minute"
    <> " (sooner if the server needs the room, so save what you need now)."

expiryMinutes :: UTCTime -> UTCTime -> Int
expiryMinutes now expiry =
  ceiling (expiry `diffUTCTime` now / 60)

expiryLine :: UTCTime -> UTCTime -> Html ()
expiryLine now expiry
  | minutes > 0 = p_ [class_ "results-expiry"] (toHtml (expirySentence minutes))
  | otherwise   = mempty
  where
    minutes = expiryMinutes now expiry

fileFilterThreshold :: Int
fileFilterThreshold = 10

fileFilterInputId :: Text
fileFilterInputId = "file-filter"

fileFilterPlaceholder :: Text
fileFilterPlaceholder = "Filter files…"

fileFilter :: Html ()
fileFilter = div_ [class_ "file-filter"] $ do
  label_ [for_ fileFilterInputId, class_ "visually-hidden"] "Filter files"
  input_
    [ type_ "search"
    , id_ fileFilterInputId
    , class_ "file-filter-input"
    , placeholder_ fileFilterPlaceholder
    , term "autocomplete" "off"
    ]
  p_ [class_ "file-filter-count", hidden_ "hidden", term "aria-live" "polite"] mempty
  p_ [class_ "file-filter-nomatch", hidden_ "hidden"] "No file matches the filter."

opensDiff :: BatchFile -> Bool
opensDiff file = case file of
  DocRow _ (Failed _) _ _ _ -> True
  DocRow _ _ _ _ hunks      -> isJust hunks
  MediaRow {}               -> False

diffPane :: Html () -> Html ()
diffPane = div_ [id_ "diff-pane", class_ "diff-pane", term "aria-live" "polite"]

plural :: Int -> Text -> Text
plural n noun = tshow n <> " " <> noun <> (if n == 1 then "" else "s")

headline :: BatchResult -> Text
headline batch
  | total == 0      = "No files were processed"
  | failed == total = "Every file failed"
  | changed == 0    = "Already up to date — nothing to change"
  | otherwise       = plural changed "file" <> " of " <> tshow total <> " updated"
  where
    total   = length (brFiles batch)
    changed = batchChangedCount batch
    failed  = batchFailedCount batch

headlineMark :: BatchResult -> Text
headlineMark batch
  | not (null (brFiles batch))
  , batchFailedCount batch == length (brFiles batch) = "🙁"
  | batchChangedCount batch > 0                      = "🎉"
  | otherwise                                        = "✨"

countPills :: BatchResult -> [(Text, Text)]
countPills batch =
  [ (cls, plural n label)
  | (cls, label, n) <-
      [ ("pill-changed",   "changed file",   batchChangedCount batch)
      , ("pill-unchanged", "unchanged file", batchUnchangedCount batch)
      , ("pill-failed",    "failed file",    batchFailedCount batch)
      ]
  , n > 0
  ]

pill :: (Text, Text) -> Html ()
pill (cls, label) = li_ [class_ ("pill " <> cls)] (toHtml label)

fileRow :: BatchId -> (Int, BatchFile) -> Html ()
fileRow batchId (index, file) = li_
  [ class_ ("file " <> statusClass status)
  , data_ "path" (T.pack path)
  ] $ do
  if opensDiff file
    then button_
      [ type_ "button"
      , class_ "file-name"
      , term "hx-get" (batchFileDiffHref batchId index Nothing)
      , term "hx-target" "#diff-pane"
      , term "aria-controls" "diff-pane"
      , title_ (T.pack path)
      ]
      (toHtml path)
    else span_ [class_ "file-name", title_ (T.pack path)]
               (toHtml path)
  span_ [class_ "badge"] (toHtml (statusLabel status))
  fileStat file
  case status of
    Failed _ -> span_ [class_ "file-download is-absent", term "aria-hidden" "true"] mempty
    _ -> a_ [ href_ (batchFileHref batchId index)
            , class_ "file-download"
            , download_ ""
            , title_ "Download just this file"
            ]
            "Download"
  where
    path = rowPath file
    status = case file of
      DocRow _ s _ _ _ -> s
      MediaRow {}      -> Unchanged

fileStat :: BatchFile -> Html ()
fileStat file = case fileHunkCounts file of
  Just (added, removed) -> span_ [class_ "file-stat"] $ do
    span_ [class_ "stat-added"] (toHtml ("+" <> tshow added))
    span_ [class_ "stat-removed"] (toHtml ("−" <> tshow removed))
  Nothing -> mempty

statusClass :: FileStatus -> Text
statusClass s = case s of
  Changed   -> "status-changed"
  Unchanged -> "status-unchanged"
  Failed _  -> "status-failed"

statusLabel :: FileStatus -> Text
statusLabel s = case s of
  Changed   -> "changed"
  Unchanged -> "unchanged"
  Failed _  -> "error"

expiredHeadline :: Text
expiredHeadline = "This result has expired"

expiredView :: Html ()
expiredView = notice "⏳" expiredHeadline $
  p_ "Results are kept for a limited time and then discarded. Please upload your files again."

nothingToZipView :: Html ()
nothingToZipView = notice "📭" nothingToZipHeadline $
  p_ "Every file in this batch failed, so the archive would be empty. Open a file in the list to read why."

nothingToZipHeadline :: Text
nothingToZipHeadline = "There is nothing to download"

downloadAllLabel :: Text
downloadAllLabel = "Download all as zip"

notice :: Text -> Text -> Html () -> Html ()
notice mark title body = div_ [class_ "notice"] $ do
  p_ [class_ "notice-mark", term "aria-hidden" "true"] (toHtml mark)
  h2_ (toHtml title)
  body
  p_ $ a_ [href_ "/", class_ "button"] "Start over"

errorBanner :: Text -> Html ()
errorBanner message =
  div_ [class_ "error-banner", term "role" "alert"] $ do
    span_ [class_ "banner-mark", term "aria-hidden" "true"] "⚠️"
    span_ [class_ "banner-message"] (toHtml message)

errorBannerTemplate :: Html ()
errorBannerTemplate = template_ [id_ errorBannerTemplateId] (errorBanner "")

errorBannerTemplateId :: Text
errorBannerTemplateId = "error-banner-template"

tshow :: Int -> Text
tshow = T.pack . show
