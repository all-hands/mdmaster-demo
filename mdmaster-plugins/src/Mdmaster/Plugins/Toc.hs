module Mdmaster.Plugins.Toc
  ( tocOperation
  , runToc
  , tocWalk
  , markerName
  ) where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.Ast (Heading (..), documentHeadings, githubSlug, headingText, nextSlug)
import Mdmaster.Core.Marker (MarkerScan (..), markerEnd, markerStart, scanMarker, wrapInMarker)
import Mdmaster.Core.Operation
  (InputFormat (..), OpError (..), Operation (..), OperationId (..))
import Mdmaster.Core.Options (OptionValues)
import Mdmaster.Plugins.Toc.Anchor (anchorBlock, isMdmasterAnchor, removeMdmasterAnchors)
import Mdmaster.Plugins.Toc.Options
  (AnchorMode (..), SkipTitle (..), TocMode (..), TocOptions (..)
  , decodeTocOptions, tocSchema)
import Mdmaster.Plugins.Toc.Render (TocEntry (..), buildTocBlocks)
import Text.Pandoc.Definition

markerName :: Text
markerName = "toc"

tocOperation :: Operation
tocOperation = Operation
  { opId          = OperationId "toc"
  , opName        = "Table of Contents"
  , opDescription =
      "Insert or update a table of contents at the top of the document, to a \
      \chosen depth. Re-running replaces the previous table rather than adding \
      \another one."
  , opInput       = MarkdownInput
  , opSchema      = tocSchema
  , opRun         = runToc
  }

runToc :: OptionValues -> Pandoc -> Either OpError Pandoc
runToc values input = case tocMode options of
    TocUpdate -> update
    TocRemove -> remove
  where
    options = decodeTocOptions values

    cleaned = removeMdmasterAnchors input

    remove = case scanMarker markerName cleanedBlocks of
      UnterminatedMarker       -> Left unterminated
      MarkerInMixedBlock       -> Left mixedBlock
      MarkerFound before after -> Right (Pandoc cleanedMeta (before <> after))
      NoMarker                 -> Right cleaned

    update = case scanMarker markerName anchored of
      UnterminatedMarker -> Left unterminated
      MarkerInMixedBlock -> Left mixedBlock
      MarkerFound before after ->
        Right (Pandoc meta (before <> markerBlock <> after))
      NoMarker ->
        Right (Pandoc meta (insertAt (insertionPoint anchored) markerBlock anchored))

    unterminated = OpError ("found '" <> markerStart markerName
                            <> "' with no matching '" <> markerEnd markerName <> "'")

    mixedBlock = OpError ("'" <> markerStart markerName <> "' or '" <> markerEnd markerName
                          <> "' shares its block with other HTML or text; put each marker \
                             \on a line of its own, with a blank line before and after")

    Pandoc cleanedMeta cleanedBlocks = cleaned

    (anchoredDoc, entries) = tocWalk options cleaned

    markerBlock = wrapInMarker markerName (buildTocBlocks (tocStyle options) entries)

    Pandoc meta anchored = anchoredDoc

type WalkState = (Int, M.Map Text Int, [TocEntry])

tocWalk :: TocOptions -> Pandoc -> (Pandoc, [TocEntry])
tocWalk options doc@(Pandoc meta blocks) =
    (Pandoc meta outBlocks, reverse entries)
  where
    depth = tocDepth options
    mode  = tocAnchor options
    omit  = omitTitle options (documentHeadings doc)

    (outBlocks, (_, _, entries)) = runState (goBlocks blocks) (0, M.empty, [])

    goBlocks :: [Block] -> State WalkState [Block]
    goBlocks = fmap concat . mapM goBlock

    goBlock :: Block -> State WalkState [Block]
    goBlock block = case block of
      Header lvl (ident, _, _) inlines -> do
        (i, seen, acc) <- get
        let raw = githubSlug (headingText (Heading lvl inlines))
            (seen', slug) = nextSlug seen raw
            target = case mode of
              AnchorPandoc | not (T.null ident) -> ident
              _                                 -> slug
            included = lvl <= depth && not (i == 0 && omit)
            entry | included  = [TocEntry lvl ("#" <> target) inlines]
                  | otherwise = []
        put ( i + 1
            , seen'
            , entry <> acc )
        pure (if mode == AnchorExplicit && included
                then [anchorBlock slug, block]
                else [block])
      Div attr inner        -> one . Div attr <$> goBlocks inner
      BlockQuote inner      -> one . BlockQuote <$> goBlocks inner
      BulletList items      -> one . BulletList <$> mapM goBlocks items
      OrderedList attrs its -> one . OrderedList attrs <$> mapM goBlocks its
      DefinitionList defs   -> one . DefinitionList <$> mapM goDefinition defs
      Figure attr caption b -> one . Figure attr caption <$> goBlocks b
      _                     -> pure [block]

    one :: a -> [a]
    one x = [x]

    goDefinition :: ([Inline], [[Block]]) -> State WalkState ([Inline], [[Block]])
    goDefinition (term, definitions) = (,) term <$> mapM goBlocks definitions

insertionPoint :: [Block] -> Int
insertionPoint blocks = case blocks of
  (b : Header 1 _ _ : _) | isMdmasterAnchor b -> 2
  (Header 1 _ _ : _)                         -> 1
  _                                          -> 0

insertAt :: Int -> [a] -> [a] -> [a]
insertAt i new xs = let (before, after) = splitAt i xs in before <> new <> after

omitTitle :: TocOptions -> [Heading] -> Bool
omitTitle options headings = case headings of
  (first : _)
    | hLevel first == 1 -> case tocSkipTitle options of
        SkipAlways -> True
        SkipAuto   -> length (filter ((== 1) . hLevel) headings) == 1
        SkipNever  -> False
  _ -> False
