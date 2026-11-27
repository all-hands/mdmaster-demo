{-# LANGUAGE FlexibleContexts #-}

module Mdmaster.Plugins.Docx.Collect
  ( Comment (..)
  , Thread (..)
  , collect
  , placeholderNote
  , placeholderNoteTail
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Mdmaster.Core.CommentSpan
  (authorKey, dateKey, dateUtcKey
  , deletionClass, doneKey, doneValue, durableIdKey, idKey, initialsKey
  , insertionClass, isCommentEnd, isCommentMarker, isCommentStart
  , parentKey, spanTransparent
  )
import Mdmaster.Core.DocxImage (isExtracted, unmarkExtracted)
import Text.Pandoc.Definition
  ( Block (..), Caption (..), Cell (..), Inline (..), Pandoc (..), Row (..)
  , TableBody (..), TableFoot (..), TableHead (..)
  )
import Text.Pandoc.Walk (Walkable, walk)

data Comment = Comment
  { cId        :: Text
  , cDurableId :: Text
  , cAuthor    :: Text
  , cDate      :: Text
  , cDateLocal :: Text
  , cInitials  :: Text
  , cDone      :: Bool
  , cParent    :: Maybe Text
  , cBody      :: [Inline]
  }
  deriving (Eq, Show)

data Thread = Thread
  { tBlockIndex :: Int
  , tAnchor     :: [Inline]
  , tRoot       :: Comment
  , tReplies    :: [Comment]
  }
  deriving (Eq, Show)

collect :: Pandoc -> (Pandoc, [Thread])
collect (Pandoc meta blocks) = (Pandoc meta cleaned, threads)
  where
    found = concat [harvest i block | (i, block) <- zip [0 ..] blocks]
    cleaned = map clean blocks
    threads = assemble found

data Found = Found
  { fBlockIndex :: Int
  , fComment    :: Comment
  , fAnchor     :: [Inline]
  }

harvest :: Int -> Block -> [Found]
harvest ix block =
    [ Found ix comment (clean (reverse (M.findWithDefault [] (cId comment) anchors)))
    | comment <- reverse comments
    ]
  where
    (comments, _, anchors) = foldl' step ([], S.empty, M.empty) (blockInlines block)

    step (acc, open, anchors') inline = case inline of
      Span (_, _, kvs) body
        | isCommentStart inline ->
            ( commentFrom kvs body : acc
            , S.insert (attr idKey kvs) open
            , M.insert (attr idKey kvs) [] anchors'
            )
        | isCommentEnd inline ->
            (acc, foldr S.delete open (endIds inline), anchors')
      _ -> (acc, open, foldr (M.adjust (inline :)) anchors' open)

    attr k kvs = fromMaybe "" (lookup k kvs)

    commentFrom kvs body = Comment
      { cId        = attr idKey kvs
      , cDurableId = attr durableIdKey kvs
      , cAuthor    = attr authorKey kvs
      , cDate      = fromMaybe (attr dateKey kvs) (lookup dateUtcKey kvs)
      , cDateLocal = attr dateKey kvs
      , cInitials  = attr initialsKey kvs
      , cDone      = lookup doneKey kvs == Just doneValue
      , cParent    = lookup parentKey kvs
      , cBody      = clean body
      }

endIds :: Inline -> [Text]
endIds inline = case inline of
  Span (_, _, kvs) inner
    | isCommentEnd inline ->
        [v | (k, v) <- kvs, k == idKey] <> concatMap endIds inner
  _ -> []

blockInlines :: Block -> [Inline]
blockInlines = concatMap noteInlines . ownInlines

noteInlines :: Inline -> [Inline]
noteInlines inline = case inline of
  Note bs -> concatMap blockInlines bs
  _       -> expandNotes inline

expandNotes :: Inline -> [Inline]
expandNotes inline = case inline of
    Emph is        -> inPlace Emph is
    Underline is   -> inPlace Underline is
    Strong is      -> inPlace Strong is
    Strikeout is   -> inPlace Strikeout is
    Superscript is -> inPlace Superscript is
    Subscript is   -> inPlace Subscript is
    SmallCaps is   -> inPlace SmallCaps is
    Quoted q is    -> inPlace (Quoted q) is
    Cite cs is     -> inPlace (Cite cs) is
    Link a is t    -> inPlace (\is' -> Link a is' t) is
    Image a is t   -> inPlace (\is' -> Image a is' t) is
    Span _ _ | not (spanTransparent inline) -> unchanged
    Span a is      -> inPlace (Span a) is
    Note bs        -> concatMap blockInlines bs
    Str _          -> unchanged
    Code _ _       -> unchanged
    Space          -> unchanged
    SoftBreak      -> unchanged
    LineBreak      -> unchanged
    Math _ _       -> unchanged
    RawInline _ _  -> unchanged
  where
    unchanged = [inline]

    inPlace con is
      | all (\(i, is') -> [i] == is') (zip is parts) = unchanged
      | all isNote is = flat
      | any isCommentMarker flat = flat
      | otherwise = [con flat]
      where
        parts = map expandNotes is
        flat = concat parts

    isNote :: Inline -> Bool
    isNote i = case i of Note _ -> True; _ -> False

ownInlines :: Block -> [Inline]
ownInlines block = case block of
  Plain inlines        -> inlines
  Para inlines         -> inlines
  Header _ _ inlines   -> inlines
  LineBlock lns        -> concat lns
  DefinitionList items ->
    concatMap (\(term, defs) -> term <> concatMap (concatMap blockInlines) defs) items
  Div _ bs             -> concatMap blockInlines bs
  BlockQuote bs        -> concatMap blockInlines bs
  BulletList itemss    -> concatMap (concatMap blockInlines) itemss
  OrderedList _ itemss -> concatMap (concatMap blockInlines) itemss
  Table _ caption _ hd bodies ft ->
    captionInlines caption
      <> concatMap rowInlines (tableRows hd bodies ft)
  Figure _ caption bs  -> captionInlines caption <> concatMap blockInlines bs
  CodeBlock _ _        -> []
  RawBlock _ _         -> []
  HorizontalRule       -> []

captionInlines :: Caption -> [Inline]
captionInlines (Caption _ bs) = concatMap blockInlines bs

tableRows :: TableHead -> [TableBody] -> TableFoot -> [Row]
tableRows (TableHead _ hd) bodies (TableFoot _ ft) =
    hd <> concatMap bodyRows bodies <> ft
  where
    bodyRows (TableBody _ _ rowHead rows) = rowHead <> rows

rowInlines :: Row -> [Inline]
rowInlines (Row _ cells) = concatMap cellInlines cells

cellInlines :: Cell -> [Inline]
cellInlines (Cell _ _ _ _ bs) = concatMap blockInlines bs

assemble :: [Found] -> [Thread]
assemble found =
    [ Thread
        { tBlockIndex = fBlockIndex f
        , tAnchor     = fAnchor f
        , tRoot       = fComment f
        , tReplies    = M.findWithDefault [] (cId (fComment f)) replies
        }
    | f <- found
    , rootOf (cId (fComment f)) == cId (fComment f)
    ]
  where
    byId = M.fromList [(cId (fComment f), fComment f) | f <- found]

    rootOf = threadRoot byId

    replies :: Map Text [Comment]
    replies = M.fromListWith (flip (<>))
      [ (rootOf cid, [fComment f])
      | f <- found
      , let cid = cId (fComment f)
      , rootOf cid /= cid
      ]

threadRoot :: Map Text Comment -> Text -> Text
threadRoot byId cid = go S.empty cid
  where
    go :: Set Text -> Text -> Text
    go seen current
      | current `S.member` seen = cid
      | otherwise = case M.lookup current byId >>= cParent of
          Just parent | M.member parent byId -> go (S.insert current seen) parent
          _                                  -> current

clean :: (Walkable [Inline] b, Walkable Inline b) => b -> b
clean = walk resolveImage . walk stripSpans

stripSpans :: [Inline] -> [Inline]
stripSpans = concatMap one
  where
    one inline = case inline of
      Span (_, classes, _) _
        | isCommentMarker inline      -> []
        | deletionClass `elem` classes -> []
      Span (_, classes, _) body
        | insertionClass `elem` classes -> body
      _ -> [inline]

resolveImage :: Inline -> Inline
resolveImage inline = case inline of
  Image attr alt target
    | isExtracted attr          -> Image (unmarkExtracted attr) alt target
  Image _ _ (target, _)         -> placeholderNote target
  _                             -> inline

placeholderNote :: Text -> Inline
placeholderNote target = Emph [Str ("[image: " <> target <> placeholderNoteTail)]

placeholderNoteTail :: Text
placeholderNoteTail = ", not extracted]"
