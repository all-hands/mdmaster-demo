module Mdmaster.Plugins.Toc.Render
  ( TocEntry (..)
  , buildTocBlocks
  ) where

import Data.Text (Text)
import Mdmaster.Core.Ast (linkSafeInlines)
import Mdmaster.Plugins.Toc.Options (TocStyle (..))
import Text.Pandoc.Definition

data TocEntry = TocEntry
  { teLevel  :: Int
  , teTarget :: Text
  , teLabel  :: [Inline]
  }
  deriving (Eq, Show)

buildTocBlocks :: TocStyle -> [TocEntry] -> [Block]
buildTocBlocks _ [] = []
buildTocBlocks style entries = [renderForest style (toForest entries)]

data TocNode = TocNode TocEntry [TocNode]

toForest :: [TocEntry] -> [TocNode]
toForest [] = []
toForest (e : rest) =
  let (children, siblings) = span ((> teLevel e) . teLevel) rest
   in TocNode e (toForest children) : toForest siblings

renderForest :: TocStyle -> [TocNode] -> Block
renderForest style = case style of
  StyleBullets  -> BulletList . map (renderNode style)
  StyleNumbered -> OrderedList (1, Decimal, Period) . map (renderNode style)

renderNode :: TocStyle -> TocNode -> [Block]
renderNode style (TocNode e children) =
  Plain [Link nullAttr (linkSafeInlines (teLabel e)) (teTarget e, "")]
    : [renderForest style children | not (null children)]
