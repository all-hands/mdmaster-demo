module Mdmaster.Plugins.Toc.Anchor
  ( anchorBlock
  , isMdmasterAnchor
  , removeMdmasterAnchors
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.Marker (rawHtmlBlock)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

anchorBlock :: Text -> Block
anchorBlock ident = rawHtmlBlock ("<a id=\"" <> ident <> "\" data-mdmaster></a>")

isMdmasterAnchor :: Block -> Bool
isMdmasterAnchor block = case block of
  RawBlock (Format "html") txt -> isAnchorText txt
  Para inlines                 -> inlineAnchor inlines
  Plain inlines                -> inlineAnchor inlines
  _ -> False
  where
    inlineAnchor inlines = case traverse rawHtml (filter (not . blank) inlines) of
      Just parts@(_ : _) -> isAnchorText (T.concat parts)
      _                  -> False
    rawHtml (RawInline (Format "html") t) = Just t
    rawHtml _                             = Nothing
    blank i = i == Space || i == SoftBreak

isAnchorText :: Text -> Bool
isAnchorText txt =
  let stripped = T.strip txt
   in "<a " `T.isPrefixOf` stripped
        && "</a>" `T.isSuffixOf` stripped
        && any (`T.isInfixOf` stripped) ["data-mdmaster", "data-mdaster"]

removeMdmasterAnchors :: Pandoc -> Pandoc
removeMdmasterAnchors = walk (filter (not . isMdmasterAnchor) :: [Block] -> [Block])
