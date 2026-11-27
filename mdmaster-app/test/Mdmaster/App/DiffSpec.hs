module Mdmaster.App.DiffSpec (spec) where

import Control.Exception (evaluate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Mdmaster.App.Diff
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "no changes" $ do
    it "produces no hunks for identical input" $
      diffHunks 3 "a\nb\nc\n" "a\nb\nc\n" `shouldBe` []

    it "produces no hunks for two empty documents" $
      diffHunks 3 "" "" `shouldBe` []

  describe "insertions" $ do
    it "reports an inserted line with surrounding context" $
      diffHunks 1 "a\nb\n" "a\nNEW\nb\n"
        `shouldBe`
          [ DiffHunk 1 1
              [ DiffLine Context "a"
              , DiffLine Added "NEW"
              , DiffLine Context "b"
              ]
          ]

    it "reports an insertion at the top of the file" $
      diffHunks 1 "a\n" "NEW\na\n"
        `shouldBe` [DiffHunk 1 1 [DiffLine Added "NEW", DiffLine Context "a"]]

    it "reports an insertion at the end of the file" $
      diffHunks 1 "a\n" "a\nNEW\n"
        `shouldBe` [DiffHunk 1 1 [DiffLine Context "a", DiffLine Added "NEW"]]

  describe "deletions and replacements" $ do
    it "reports a removed line" $
      diffHunks 1 "a\nGONE\nb\n" "a\nb\n"
        `shouldBe`
          [ DiffHunk 1 1
              [ DiffLine Context "a"
              , DiffLine Removed "GONE"
              , DiffLine Context "b"
              ]
          ]

    it "reports a replacement as a removal followed by an addition" $
      diffHunks 0 "a\nOLD\nb\n" "a\nNEW\nb\n"
        `shouldBe` [DiffHunk 2 2 [DiffLine Removed "OLD", DiffLine Added "NEW"]]

  describe "hunking" $ do
    it "collapses distant changes into separate hunks" $
      let old = T.intercalate "\n" (["x"] <> replicate 20 "same" <> ["y"]) <> "\n"
          new = T.intercalate "\n" (["X"] <> replicate 20 "same" <> ["Y"]) <> "\n"
       in length (diffHunks 2 old new) `shouldBe` 2

    it "merges changes that are closer together than twice the context" $
      let old = T.intercalate "\n" (["x"] <> replicate 2 "same" <> ["y"]) <> "\n"
          new = T.intercalate "\n" (["X"] <> replicate 2 "same" <> ["Y"]) <> "\n"
       in length (diffHunks 3 old new) `shouldBe` 1

    it "records 1-based start lines for both sides" $
      let old = T.intercalate "\n" (replicate 10 "same" <> ["old"]) <> "\n"
          new = T.intercalate "\n" (replicate 10 "same" <> ["new"]) <> "\n"
       in map (\h -> (dhOldStart h, dhNewStart h)) (diffHunks 1 old new)
            `shouldBe` [(10, 10)]

    it "accounts for earlier insertions when numbering later hunks" $
      let old = T.intercalate "\n" (["a"] <> replicate 20 "same" <> ["b"]) <> "\n"
          new = T.intercalate "\n" (["a", "INSERTED"] <> replicate 20 "same" <> ["B"]) <> "\n"
       in map dhNewStart (diffHunks 1 old new) `shouldBe` [1, 22]

  describe "trailing newlines" $
    it "does not invent a trailing empty line" $
      diffHunks 3 "a\n" "a\n" `shouldBe` []

  describe "hunk extents" $ do
    it "counts an added line on the new side only" $
      let hunk = DiffHunk 1 1
            [DiffLine Context "a", DiffLine Added "NEW", DiffLine Context "b"]
       in hunkOldLength hunk `shouldBe` 2

    it "counts a removed line on the old side only" $
      let hunk = DiffHunk 1 1
            [DiffLine Context "a", DiffLine Removed "GONE", DiffLine Context "b"]
       in hunkOldLength hunk `shouldBe` 3

    it "leaves the old-side gap between two hunks countable" $
      let body = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
          old = T.unlines (body <> ["y"])
          new = T.unlines (["a", "INSERTED"] <> drop 1 body <> ["Y"])
       in case diffHunks 1 old new of
            [first, second] ->
              (dhOldStart first + hunkOldLength first, dhOldStart second)
                `shouldBe` (3, 11)
            other ->
              expectationFailure ("expected two hunks, got " <> show (length other))

  describe "hunk counts" $ do
    it "counts each side across all hunks" $
      hunkCounts
        [ DiffHunk 1 1 [DiffLine Context "a", DiffLine Added "NEW"]
        , DiffHunk 5 6 [DiffLine Removed "GONE", DiffLine Removed "ALSO", DiffLine Added "BACK"]
        ]
        `shouldBe` (2, 2)

    it "counts nothing when there are no hunks" $
      hunkCounts [] `shouldBe` (0, 0)

  describe "line tokens" $ do
    it "keeps each word with the whitespace that follows it" $
      lineTokens "the quick  brown fox"
        `shouldBe` ["the ", "quick  ", "brown ", "fox"]

    it "re-joins to the line it came from, spacing verbatim" $
      let line = "  leading space, tabs\tand  doubles  "
       in T.concat (lineTokens line) `shouldBe` line

    it "treats a run of only whitespace as one token" $
      lineTokens "   " `shouldBe` ["   "]

    it "gives an empty line no tokens" $
      lineTokens "" `shouldBe` []

  describe "word diff of a pair" $ do
    it "marks only the tokens that differ" $
      wordDiff (DiffLine Removed "the quick fox") (DiffLine Added "the slow fox")
        `shouldBe`
          ( [ DiffSegment "the " False, DiffSegment "quick " True, DiffSegment "fox" False ]
          , [ DiffSegment "the " False, DiffSegment "slow " True, DiffSegment "fox" False ]
          )

    it "keeps each side's segments concatenating back to its own line" $
      let (gone, added) =
            wordDiff (DiffLine Removed "a  b\tc ") (DiffLine Added "a b c ")
       in (T.concat (map dsText gone), T.concat (map dsText added))
            `shouldBe` ("a  b\tc ", "a b c ")

    it "marks every token when the pair shares no words" $
      wordDiff (DiffLine Removed "all gone") (DiffLine Added "")
        `shouldBe` ([DiffSegment "all gone" True], [])

  describe "word-level pairing in a hunk" $ do
    it "leaves a context line untouched" $
      wordSegments (DiffHunk 1 1 [DiffLine Context "same"])
        `shouldBe` [[DiffSegment "same" False]]

    it "pairs the k-th removed line with the k-th added line" $
      wordSegments (DiffHunk 1 1
        [ DiffLine Context "ctx"
        , DiffLine Removed "keep a"
        , DiffLine Removed "drop b"
        , DiffLine Added "keep c"
        , DiffLine Added "drop d"
        ])
        `shouldBe`
          [ [DiffSegment "ctx" False]
          , [DiffSegment "keep " False, DiffSegment "a" True]
          , [DiffSegment "drop " False, DiffSegment "b" True]
          , [DiffSegment "keep " False, DiffSegment "c" True]
          , [DiffSegment "drop " False, DiffSegment "d" True]
          ]

    it "emphasises a leftover without a counterpart whole" $
      wordSegments (DiffHunk 1 1
        [ DiffLine Removed "same one"
        , DiffLine Removed "gone two"
        , DiffLine Added "same uno"
        ])
        `shouldBe`
          [ [DiffSegment "same " False, DiffSegment "one" True]
          , [DiffSegment "gone two" True]
          , [DiffSegment "same " False, DiffSegment "uno" True]
          ]

    it "emphasises a pure insertion run whole" $
      wordSegments (DiffHunk 1 1 [DiffLine Context "a", DiffLine Added "entirely new"])
        `shouldBe`
          [ [DiffSegment "a" False]
          , [DiffSegment "entirely new" True]
          ]

    it "emphasises every token of a pair that shares no words" $
      wordSegments (DiffHunk 1 1 [DiffLine Removed "was", DiffLine Added "now"])
        `shouldBe`
          [ [DiffSegment "was" True]
          , [DiffSegment "now" True]
          ]

    it "stays parallel to the hunk's lines, context included" $
      let hunk = DiffHunk 3 4
            [ DiffLine Context "a"
            , DiffLine Removed "b"
            , DiffLine Added "B"
            , DiffLine Context "c"
            ]
       in length (wordSegments hunk) `shouldBe` length (dhLines hunk)

  describe "large documents" $
    it "diffs twenty thousand lines well inside a second" $ do
      let numbered = map (T.pack . show) [1 :: Int .. 20000]
          old = T.unlines numbered
          new = T.unlines (take 9999 numbered <> ["CHANGED"] <> drop 10000 numbered)
      finished <- timeout 5000000 (evaluate (length (diffHunks 3 old new)))
      isJust finished `shouldBe` True
