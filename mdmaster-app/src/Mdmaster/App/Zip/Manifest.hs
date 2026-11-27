module Mdmaster.App.Zip.Manifest
  ( manifestName
  , mdmasterManifestName
  , manifestCandidates
  , manifestEntryPath
  , manifestFor
  , statusWord
  , utcStamp
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Mdmaster.App.Types
import Mdmaster.Core.Operation (OperationId (..))
import Mdmaster.Core.Options (OptionValue (..), OptionValues (..))

manifestName :: FilePath
manifestName = "MANIFEST.md"

mdmasterManifestName :: FilePath
mdmasterManifestName = "mdmaster-MANIFEST.md"

manifestCandidates :: [FilePath]
manifestCandidates =
  manifestName : mdmasterManifestName : map numberedManifestName [2 ..]

numberedManifestName :: Int -> FilePath
numberedManifestName n = "mdmaster-MANIFEST-" <> show n <> ".md"

manifestEntryPath :: [FilePath] -> FilePath
manifestEntryPath taken =
  case filter free [manifestName, mdmasterManifestName] of
    candidate : _ -> candidate
    []            -> numbered 2
  where
    free = (`notElem` taken)
    numbered n
      | free (numberedManifestName n) = numberedManifestName n
      | otherwise                     = numbered (n + 1)

utcStamp :: UTCTime -> Text
utcStamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

manifestFor :: BatchResult -> Text
manifestFor batch = T.unlines $
  [ "# mdmaster batch manifest"
  , ""
  , "- Operation: " <> unOperationId (brOperation batch)
  , "- Ran at: " <> utcStamp (brAt batch)
  , "- Options: " <> optionsText (brOptions batch)
  , ""
  , "| Path | Status | Added | Removed |"
  , "|---|---|---|---|"
  ]
    ++ [ "| " <> T.intercalate " | " (cells file) <> " |" | file <- brFiles batch ]
  where
    cells file = cell (T.pack (rowPath file)) : statusCell file : countCells file
    statusCell file = case file of
      DocRow _ status _ _ _ -> statusWord status
      MediaRow {}           -> statusWord Unchanged

cell :: Text -> Text
cell = T.replace "|" "\\|" . T.replace "\\" "\\\\"

countCells :: BatchFile -> [Text]
countCells file = case fileHunkCounts file of
  Just (added, removed) -> [tshow added, tshow removed]
  Nothing               -> ["", ""]

optionsText :: OptionValues -> Text
optionsText (OptionValues fields)
  | M.null fields = "none"
  | otherwise     = T.intercalate ", "
      [ key <> ": " <> valueText value | (key, value) <- M.toList fields ]

valueText :: OptionValue -> Text
valueText value = case value of
  VInt n     -> tshow n
  VEnum wire -> wire

statusWord :: FileStatus -> Text
statusWord status = case status of
  Changed   -> "changed"
  Unchanged -> "unchanged"
  Failed _  -> "failed"

tshow :: Int -> Text
tshow = T.pack . show
