module Mdmaster.Plugins.Docx.Render
  ( Placement (..)
  , renderComments
  , agentMeta
  , inlineText
  , markerName
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.CommentSpan
  (authorKey, doneKey, doneValue, durableIdKey, initialsKey, parentKey)
import Mdmaster.Core.Marker (markerPrefix, rawHtmlBlock, wrapInMarker)
import Mdmaster.Plugins.Docx.Collect (Comment (..), Thread (..))
import Mdmaster.Plugins.Docx.Options (CommentStyle (..))
import Text.Pandoc.Definition (Block (..), Inline (..), nullAttr)

data Placement
  = Trailing [Block]
  | Inline [(Int, [Block])]
  deriving (Eq, Show)

markerName :: Text
markerName = "docx-comments"

renderComments :: CommentStyle -> [Thread] -> Placement
renderComments style [] = case style of
  Review  -> Inline []
  Archive -> Inline []
  _       -> Trailing []
renderComments style threads = case style of
  DiffStable -> Trailing (wrapInMarker markerName (heading : concatMap diffStable threads))
  Agent      -> Trailing (wrapInMarker markerName (concatMap agent threads))
  Review     -> Inline [(tBlockIndex t, review t) | t <- threads]
  Archive    -> Inline [(tBlockIndex t, archive t) | t <- threads]
  where
    heading = Header 2 nullAttr [Str "Comments"]

agentMeta :: [Thread] -> [(Text, Text)]
agentMeta threads =
  [ ("comments",   tshow (sum [1 + length (tReplies t) | t <- threads]))
  , ("threads",    tshow (length threads))
  , ("unresolved", tshow (length [t | t <- threads, not (cDone (tRoot t))]))
  ]

diffStable :: Thread -> [Block]
diffStable thread =
  Header 3 nullAttr (anchorHeading thread)
    : [BulletList [[Plain (line c)] | c <- tRoot thread : tReplies thread]]
  where
    line c = [Strong [Str (cAuthor c)], Space, Str (paren (day c) <> ":"), Space] <> cBody c

review :: Thread -> [Block]
review thread = [BlockQuote (header : Para (cBody root) : map reply (tReplies thread))]
  where
    root = tRoot thread
    header = Para
      ( [Strong [Str ("\128172 " <> cAuthor root)], Str (" \183 " <> day root)]
          <> (if cDone root then [Str " \183 resolved"] else [])
          <> maybe [] quoted (anchorText thread)
      )
    quoted t = [SoftBreak, Str "on ", Str (quote t)]
    reply c = BlockQuote
      [ Para [Strong [Str ("\8627 " <> cAuthor c)], Str (" \183 " <> day c)]
      , Para (cBody c)
      ]

agent :: Thread -> [Block]
agent thread =
    Header 3 nullAttr [Str ("c:" <> threadKey root)]
      : [ BulletList
            ( [ [Plain [Str ("anchor: " <> maybe "none" quote (anchorText thread))]]
              , [Plain [Str ("resolved: " <> lower (cDone root))]]
              ]
                <> [entry "comment" root]
                <> [entry "reply" c | c <- tReplies thread]
            )
        ]
  where
    root = tRoot thread
    entry label c =
      [ Plain [Str (label <> ": author=" <> quote (cAuthor c)
                          <> " date=" <> quote (cDate c))]
      , BlockQuote [Para (cBody c)]
      ]

threadKey :: Comment -> Text
threadKey c
  | T.null (cDurableId c) = cId c
  | otherwise             = cDurableId c

archive :: Thread -> [Block]
archive thread = concatMap one (tRoot thread : tReplies thread)
  where
    anchor = inlineText (tAnchor thread)
    one c =
      [ rawHtmlBlock (T.concat
          [ markerPrefix <> "comment"
          , field "id" (cId c)
          , field durableIdKey (cDurableId c)
          , field authorKey (cAuthor c)
          , field initialsKey (cInitials c)
          , field "date" (cDate c)
          , field "dateLocal" (cDateLocal c)
          , field parentKey (fromMaybe "" (cParent c))
          , field doneKey (if cDone c then doneValue else "false")
          , field "anchor" anchor
          , " -->"
          ])
      , Para (cBody c)
      ]

field :: Text -> Text -> Text
field key value = " " <> key <> "=\"" <> escape value <> "\""

escape :: Text -> Text
escape =
    T.replace "-" "\\-"
  . T.replace "\"" "\\\""
  . T.replace "\\" "\\\\"

anchorText :: Thread -> Maybe Text
anchorText thread
  | T.null flattened = Nothing
  | otherwise        = Just flattened
  where
    flattened = T.strip (inlineText (tAnchor thread))

anchorHeading :: Thread -> [Inline]
anchorHeading thread = case anchorText thread of
  Just _  -> tAnchor thread
  Nothing -> [Str "(no anchored text)"]

day :: Comment -> Text
day = T.takeWhile (/= 'T') . cDate

paren :: Text -> Text
paren t = "(" <> t <> ")"

quote :: Text -> Text
quote t = "\"" <> T.replace "\"" "'" t <> "\""

lower :: Bool -> Text
lower b = if b then doneValue else "false"

tshow :: Int -> Text
tshow = T.pack . show

inlineText :: [Inline] -> Text
inlineText = T.concat . map go
  where
    go inline = case inline of
      Str s          -> s
      Space          -> " "
      SoftBreak      -> " "
      LineBreak      -> " "
      Code _ s       -> s
      Emph is        -> inlineText is
      Underline is   -> inlineText is
      Strong is      -> inlineText is
      Strikeout is   -> inlineText is
      Superscript is -> inlineText is
      Subscript is   -> inlineText is
      SmallCaps is   -> inlineText is
      Quoted _ is    -> inlineText is
      Cite _ is      -> inlineText is
      Link _ is _    -> inlineText is
      Span _ is      -> inlineText is
      Image _ is _   -> inlineText is
      Note _         -> ""
      RawInline _ _  -> ""
      Math _ _       -> ""
