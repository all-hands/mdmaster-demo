module Mdmaster.Web.View.Diff
  ( diffView
  , countKind
  , wordChangedClass
  , diffTitleClass
  , copyLabel
  , copyClass
  , copyHrefAttribute
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Mdmaster.App.Diff
  ( DiffHunk (..)
  , DiffKind (..)
  , DiffLine (..)
  , DiffSegment (..)
  , hunkOldLength
  , wordSegments
  )
import Mdmaster.App.Types
import Mdmaster.Web.Api (batchFileHref)
import Mdmaster.Web.View.DiffContext (contextControl)

diffView :: BatchId -> Int -> Int -> BatchFile -> Html ()
diffView batchId index context file = div_ [class_ "diff"] $ do
  div_ [class_ "diff-head"] $ do
    h2_ [class_ diffTitleClass, tabindex_ "-1"] (toHtml (rowPath file))
    case file of
      DocRow _ Changed _ _ _ -> do
        p_ [class_ "diff-stat"] $ do
          span_ [class_ "stat-added"] (toHtml ("+" <> tshow (countKind Added file)))
          span_ [class_ "stat-removed"] (toHtml ("−" <> tshow (countKind Removed file)))
        contextControl batchId index context
      _ -> mempty
    copyButton batchId index file
  case file of
    DocRow _ (Failed message) _ _ _ -> p_ [class_ "file-error"] (toHtml message)
    DocRow _ Unchanged _ _ _        -> unchangedNote
    DocRow _ Changed _ _ hunks      -> case hunks of
      Just hs -> table_ [class_ "diff-table"] $
                   mapM_ hunkRows (withGaps hs)
      Nothing -> mempty
    MediaRow {}                     -> unchangedNote
  where
    unchangedNote = p_ [class_ "unchanged-note"] $ do
      span_ [class_ "placeholder-mark", term "aria-hidden" "true"] "👌"
      span_ "This file already looks exactly as the operation would leave it."

countKind :: DiffKind -> BatchFile -> Int
countKind kind file = case fileHunkCounts file of
  Nothing -> 0
  Just (added, removed) -> case kind of
    Added   -> added
    Removed -> removed
    Context -> 0

diffTitleClass :: Text
diffTitleClass = "diff-title"

copyButton :: BatchId -> Int -> BatchFile -> Html ()
copyButton batchId index file = case file of
  DocRow _ _ (Just _) _ _ ->
    button_
      [ type_ "button"
      , class_ copyClass
      , term copyHrefAttribute (batchFileHref batchId index)
      ]
      (toHtml copyLabel)
  DocRow {}   -> mempty
  MediaRow {} -> mempty

copyLabel :: Text
copyLabel = "Copy"

copyClass :: Text
copyClass = "diff-copy"

copyHrefAttribute :: Text
copyHrefAttribute = "data-copy-href"

withGaps :: [DiffHunk] -> [(Maybe DiffHunk, DiffHunk)]
withGaps hunks = zip (Nothing : map Just hunks) hunks

hunkRows :: (Maybe DiffHunk, DiffHunk) -> Html ()
hunkRows (previous, hunk) = do
  mapM_ gapRow (elidedRange previous hunk)
  mapM_ lineRow (zip (numberLines hunk) (wordSegments hunk))

elidedRange :: Maybe DiffHunk -> DiffHunk -> Maybe (Int, Int)
elidedRange previous hunk = do
  before <- previous
  let resumeAt = dhOldStart before + hunkOldLength before
  if dhOldStart hunk > resumeAt
    then Just (resumeAt, dhOldStart hunk - 1)
    else Nothing

gapRow :: (Int, Int) -> Html ()
gapRow (from, to) =
  tr_ [class_ "diff-gap"] $ td_ [colspan_ "4"] $ toHtml $
    if from == to
      then "⋯ line " <> tshow from <> " unchanged ⋯"
      else "⋯ lines " <> tshow from <> "–" <> tshow to <> " unchanged ⋯"

numberLines :: DiffHunk -> [(Text, Text, DiffLine)]
numberLines hunk = go (dhOldStart hunk) (dhNewStart hunk) (dhLines hunk)
  where
    go _ _ [] = []
    go o n (l : ls) = case dlKind l of
      Context -> (tshow o, tshow n, l) : go (o + 1) (n + 1) ls
      Removed -> (tshow o, "",      l) : go (o + 1) n ls
      Added   -> ("",      tshow n, l) : go o (n + 1) ls

lineRow :: ((Text, Text, DiffLine), [DiffSegment]) -> Html ()
lineRow ((oldNo, newNo, line), segments) =
  tr_ [class_ (kindClass (dlKind line))] $ do
    td_ [class_ "lineno"] (toHtml oldNo)
    td_ [class_ "lineno"] (toHtml newNo)
    td_ [class_ "linemark", term "aria-hidden" "true"] (toHtml (kindMark (dlKind line)))
    td_ [class_ "linetext"] (mapM_ lineSegment segments)

lineSegment :: DiffSegment -> Html ()
lineSegment s
  | dsChanged s = span_ [class_ wordChangedClass] (toHtml (dsText s))
  | otherwise   = toHtml (dsText s)

wordChangedClass :: Text
wordChangedClass = "word-changed"

kindClass :: DiffKind -> Text
kindClass k = case k of
  Context -> "line-context"
  Added   -> "line-added"
  Removed -> "line-removed"

kindMark :: DiffKind -> Text
kindMark k = case k of
  Context -> ""
  Added   -> "+"
  Removed -> "−"

tshow :: Int -> Text
tshow = T.pack . show
