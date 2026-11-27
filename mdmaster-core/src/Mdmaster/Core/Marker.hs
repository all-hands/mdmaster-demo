module Mdmaster.Core.Marker
  ( markerStart
  , markerEnd
  , markerPrefix
  , legacyMarkerPrefix
  , carriesMarkerPrefix
  , MarkerScan (..)
  , scanMarker
  , wrapInMarker
  , rawHtmlBlock
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..))
import Text.Pandoc.Walk (query)

markerPrefix :: Text
markerPrefix = "<!-- mdmaster:"

legacyMarkerPrefix :: Text
legacyMarkerPrefix = "<!-- mdaster:"

carriesMarkerPrefix :: Text -> Bool
carriesMarkerPrefix txt = any (`T.isInfixOf` txt) [markerPrefix, legacyMarkerPrefix]

markerStart :: Text -> Text
markerStart name = markerPrefix <> name <> " start -->"

markerEnd :: Text -> Text
markerEnd name = markerPrefix <> name <> " end -->"

spellings :: (Text -> Text) -> [Text]
spellings line = [line markerPrefix, line legacyMarkerPrefix]

data MarkerScan
  = NoMarker
  | MarkerFound [Block] [Block]
  | UnterminatedMarker
  | MarkerInMixedBlock
  deriving (Eq, Show)

scanMarker :: Text -> [Block] -> MarkerScan
scanMarker name blocks =
  case break (rawHas starts) blocks of
    (_, []) -> NoMarker
    (before, startBlock : rest)
      | mixed startBlock -> MarkerInMixedBlock
      | rawHas ends startBlock -> MarkerFound before rest
      | otherwise ->
          case break endsHere rest of
            (_, [])                -> UnterminatedMarker
            (_, endBlock : after)
              | endIsLast endBlock -> MarkerFound before after
              | otherwise          -> MarkerInMixedBlock
  where
    starts = spellings (\p -> p <> name <> " start -->")
    ends = spellings (\p -> p <> name <> " end -->")
    hasAny lits txt = any (`T.isInfixOf` txt) lits
    rawHas lits block = case block of
      RawBlock (Format fmt) txt -> fmt == "html" && hasAny lits txt
      _                         -> False
    mixed block = case block of
      RawBlock _ txt -> not (T.null (T.strip (foldr (`T.replace` "") txt (starts <> ends))))
      _              -> False
    endsHere block = rawHas ends block || any isEndInline (inlinesOf block)
    endIsLast block = case block of
      RawBlock _ _ -> not (mixed block)
      _            -> case reverse (filter (not . blank) (inlinesOf block)) of
        lastInline : _ -> isEndInline lastInline
        []             -> False
    isEndInline (RawInline (Format "html") txt) = hasAny ends txt
    isEndInline _                               = False
    inlinesOf :: Block -> [Inline]
    inlinesOf = query (: [])
    blank i = i == Space || i == SoftBreak || i == LineBreak

wrapInMarker :: Text -> [Block] -> [Block]
wrapInMarker name content =
  [rawHtmlBlock (markerStart name)] <> content <> [rawHtmlBlock (markerEnd name)]

rawHtmlBlock :: Text -> Block
rawHtmlBlock = RawBlock (Format "html")
