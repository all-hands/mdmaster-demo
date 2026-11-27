module Mdmaster.Web.View.DiffContextSpec (spec) where

import qualified Data.Text as T
import Mdmaster.App.Diff (DiffHunk (..), DiffKind (..), DiffLine (..), diffHunks)
import Mdmaster.App.Pipeline (diffContext)
import Mdmaster.Web.View.DiffContext
import Test.Hspec

spec :: Spec
spec = do
  describe "parseDiffContext" $ do
    it "reads a plain number" $
      parseDiffContext "3" `shouldBe` Just 3

    it "refuses anything that is not a whole number" $ do
      parseDiffContext "standard" `shouldBe` Nothing
      parseDiffContext ""         `shouldBe` Nothing
      parseDiffContext "1.5"      `shouldBe` Nothing
      parseDiffContext "3 apples" `shouldBe` Nothing

    it "forgives the surrounding whitespace the reader lexes off" $ do
      parseDiffContext " 3" `shouldBe` Just 3
      parseDiffContext "3 " `shouldBe` Just 3

  describe "clampDiffContext" $
    it "folds a value into the range the route serves" $ do
      clampDiffContext 0              `shouldBe` 0
      clampDiffContext diffContext    `shouldBe` diffContext
      clampDiffContext diffContextCap `shouldBe` diffContextCap
      clampDiffContext 100000         `shouldBe` diffContextCap
      clampDiffContext (-5)           `shouldBe` 0

  describe "diffContextChoices" $
    it "names the standard and the cap, each with its label" $ do
      map fst diffContextChoices `shouldBe` [0, diffContext, 10, diffContextCap]
      lookup diffContext    diffContextChoices `shouldBe` Just "standard"
      lookup diffContextCap diffContextChoices `shouldBe` Just "50 lines"
      lookup 0  diffContextChoices `shouldBe` Just "less"
      lookup 10 diffContextChoices `shouldBe` Just "more"

  describe "the cap" $
    it "merges a document's changes into one hunk spanning it" $
      let same = replicate 20 "same"
          old  = T.unlines (same <> ["old"])
          new  = T.unlines (same <> ["new"])
       in diffHunks diffContextCap old new
            `shouldBe` [ DiffHunk 1 1
                           (replicate 20 (DiffLine Context "same")
                              <> [DiffLine Removed "old", DiffLine Added "new"])
                       ]
