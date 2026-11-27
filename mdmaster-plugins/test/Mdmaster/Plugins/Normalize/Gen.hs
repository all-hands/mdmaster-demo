module Mdmaster.Plugins.Normalize.Gen
  ( SpicyDoc (..)
  ) where

import Data.Text (Text)
import Test.QuickCheck
import Text.Pandoc.Definition

newtype SpicyDoc = SpicyDoc Pandoc
  deriving Show

fragments :: [Text]
fragments =
  [ "he said \"hi\""
  , "don't"
  , "'quoted'"
  , "a -- b"
  , "a --- b"
  , "wait..."
  , "\"unpaired"
  , "'tis"
  , "\" end\""
  , "plain"
  ]

coded :: [Text]
coded = ["\"coded\"", "'coded'", "--not prose--"]

autolinkUrls :: [Text]
autolinkUrls =
  [ "https://example.com/a--b"
  , "https://example.com/'x'"
  , "https://example.com/\"q\"...z"
  , "https://example.com/---"
  ]

genAutolink :: Gen Inline
genAutolink = oneof
  [ (\u -> Link ("", ["uri"], []) [Str u] (u, "")) <$> elements autolinkUrls
  , pure (Link ("", ["email"], []) [Str "a--b@example.com"]
                ("mailto:a--b@example.com", ""))
  ]

genInline :: Int -> Gen Inline
genInline 0 = oneof [Str <$> elements fragments, pure Space]
genInline depth = frequency
  [ (6, Str <$> elements fragments)
  , (2, pure Space)
  , (1, pure SoftBreak)
  , (1, Code ("", [], []) <$> elements coded)
  , (1, Emph . pure <$> genInline (depth - 1))
  , (1, link <$> genInline (depth - 1))
  , (1, genAutolink)
  ]
  where
    link inner = Link nullAttr [inner] ("https://example.com", "")

instance Arbitrary SpicyDoc where
  arbitrary = do
    depth <- choose (1, 3)
    n <- choose (0, 12)
    inlines <- vectorOf n (genInline depth)
    pure (SpicyDoc (Pandoc nullMeta [Para inlines]))
