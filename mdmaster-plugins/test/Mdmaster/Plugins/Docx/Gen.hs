module Mdmaster.Plugins.Docx.Gen
  ( EnrichedDoc (..)
  , start
  , end
  , commented
  ) where

import Data.Text (Text)
import Mdmaster.Core.CommentSpan
  ( authorKey
  , commentEndClass
  , commentStartClass
  , deletionClass
  , doneKey
  , doneValue
  , durableIdKey
  , idKey
  , initialsKey
  , insertionClass
  , parentKey
  )
import Test.QuickCheck
import Text.Pandoc.Definition

newtype EnrichedDoc = EnrichedDoc Pandoc

start :: Text -> Text -> [(Text, Text)] -> Text -> Inline
start cid author extra body =
  Span ("", [commentStartClass], [(idKey, cid), (authorKey, author)] <> extra)
    [Str body]

end :: [Text] -> Inline
end closed = case closed of
  []       -> Str ""
  (i : is) -> Span ("", [commentEndClass], [(idKey, i)]) [end is | not (null is)]

commented :: Text -> Text -> Block
commented cid word = Para [start cid "A" [] "note", Str word, end [cid]]

instance Show EnrichedDoc where
  show (EnrichedDoc d) = show d

ids :: [Text]
ids = ["0", "1", "2"]

authors :: [Text]
authors = ["Ada", "Grace"]

prose :: [Text]
prose = ["alpha", "beta", "gamma", "delta"]

commentStart :: Gen Inline
commentStart = do
  cid     <- elements ids
  author  <- elements authors
  parent  <- frequency [(3, pure Nothing), (1, Just <$> elements ids)]
  done    <- arbitrary
  body    <- genInlines 0
  let base = [ (idKey, cid)
             , (authorKey, author)
             , (initialsKey, "A")
             , (durableIdKey, "d" <> cid)
             ]
      extra = [(doneKey, doneValue) | done] <> maybe [] (\p -> [(parentKey, p)]) parent
  pure (Span ("", [commentStartClass], base <> extra) body)

commentEnd :: Gen Inline
commentEnd = do
  n   <- choose (1, 3)
  cid <- elements ids
  is  <- vectorOf (n - 1) (elements ids)
  pure (nest (cid : is))
  where
    nest []      = Str ""
    nest (x : xs) = Span ("", [commentEndClass], [(idKey, x)]) [nest xs | not (null xs)]

insertion :: Int -> Gen Inline
insertion depth = Span ("", [insertionClass], []) <$> genInlines depth

deletion :: Int -> Gen Inline
deletion depth = Span ("", [deletionClass], []) <$> genInlines depth

genInline :: Int -> Gen Inline
genInline 0 =
  frequency
    [ (4, Str <$> elements prose)
    , (1, pure Space)
    , (1, Code nullAttr <$> elements ["x", "y"])
    ]
genInline depth =
  frequency
    [ (4, Str <$> elements prose)
    , (2, pure Space)
    , (1, Code nullAttr <$> elements ["x", "y"])
    , (1, Emph <$> genInlines (depth - 1))
    , (2, commentStart)
    , (2, commentEnd)
    , (1, insertion (depth - 1))
    , (1, deletion (depth - 1))
    , (1, pure (Image nullAttr [Str "pic"] ("media/pic.png", "fig")))
    , (1, pure (Note [Plain [Str "in note"]]))
    ]

genInlines :: Int -> Gen [Inline]
genInlines depth = do
  n <- choose (0, 8)
  vectorOf n (genInline depth)

cellRow :: [Block] -> Row
cellRow bs = Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) bs]

genBlock :: Int -> Gen Block
genBlock depth =
  frequency
    [ (5, Para <$> genInlines depth)
    , (2, Plain <$> genInlines depth)
    , (1, Header <$> choose (1, 6) <*> pure ("", [], []) <*> genInlines 0)
    , (1, BlockQuote . pure . Para <$> genInlines (depth - 1))
    , (1, BulletList . pure . pure . Para <$> genInlines (depth - 1))
    , (1, do
          inlines <- genInlines depth
          pure (Table nullAttr (Caption Nothing []) []
                  (TableHead nullAttr [])
                  [TableBody nullAttr (RowHeadColumns 0) [] [cellRow [Plain inlines]]]
                  (TableFoot nullAttr [])))
    , (1, Div nullAttr . pure . Para <$> genInlines (depth - 1))
    ]

instance Arbitrary EnrichedDoc where
  arbitrary = do
    n <- choose (0, 5)
    blocks <- vectorOf n (genBlock 2)
    pure (EnrichedDoc (Pandoc nullMeta blocks))
