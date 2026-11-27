module Mdmaster.Plugins.Toc.Gen
  ( DocAndOptions (..)
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Marker (wrapInMarker)
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.Toc (markerName)
import Mdmaster.Plugins.Toc.Anchor (anchorBlock)
import Mdmaster.Plugins.Toc.Options (tocSchema)
import Test.QuickCheck
import Text.Pandoc.Definition

data DocAndOptions = DocAndOptions Pandoc OptionValues

instance Show DocAndOptions where
  show (DocAndOptions d _) = show d

titles :: [Text]
titles = ["Intro", "Intro", "Usage & Setup", "API", "Notes", "C++", "!!!"]

genBlock :: Gen Block
genBlock = frequency
  [ (5, Header <$> choose (1, 6) <*> pure ("", [], []) <*> (pure . Str <$> elements titles))
  , (3, pure (Para [Str "body text"]))
  , (1, pure (CodeBlock ("", [], []) "code"))
  , (1, pure (BlockQuote [Para [Str "quoted"]]))
  , (1, pure (anchorBlock "stale"))
  ]

genBlocks :: Gen [Block]
genBlocks = do
  n <- choose (0, 10)
  vectorOf n genBlock

withMarker :: [Block] -> Gen [Block]
withMarker blocks = frequency
  [ (2, pure blocks)
  , (1, do
        i <- choose (0, length blocks)
        let (before, after) = splitAt i blocks
            markerBlock = wrapInMarker markerName [BulletList [[Plain [Str "stale entry"]]]]
        pure (before <> markerBlock <> after))
  ]

genOptions :: Gen OptionValues
genOptions = do
  mode   <- elements ["update", "remove"]
  depth  <- elements ["1", "2", "3", "4", "5", "6"]
  skip   <- elements ["auto", "always", "never"]
  anchor <- elements ["github", "pandoc", "explicit"]
  style  <- elements ["bullets", "numbered"]
  let raw = M.fromList
        [ ("mode", mode), ("depth", depth), ("skipTitle", skip)
        , ("anchors", anchor), ("style", style) ]
  case validateOptions tocSchema raw of
    Left errs -> error ("generator produced invalid options: " <> show errs)
    Right vs  -> pure vs

instance Arbitrary DocAndOptions where
  arbitrary = do
    blocks <- genBlocks >>= withMarker
    DocAndOptions (Pandoc nullMeta blocks) <$> genOptions
