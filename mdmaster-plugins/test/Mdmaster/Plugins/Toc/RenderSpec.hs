module Mdmaster.Plugins.Toc.RenderSpec (spec) where

import Data.Text (Text)
import Mdmaster.Plugins.Toc.Options (TocStyle (..))
import Mdmaster.Plugins.Toc.Render
import Test.Hspec
import Text.Pandoc.Definition

numAttrs :: ListAttributes
numAttrs = (1, Decimal, Period)

entry :: Int -> Text -> Text -> TocEntry
entry lvl target label = TocEntry lvl target [Str label]

link :: Text -> Text -> Block
link target label = Plain [Link nullAttr [Str label] (target, "")]

spec :: Spec
spec = do
  describe "bullets" $ do
    it "renders nothing for no entries" $
      buildTocBlocks StyleBullets [] `shouldBe` []

    it "renders a flat list of siblings" $
      buildTocBlocks StyleBullets [entry 2 "#a" "A", entry 2 "#b" "B"]
        `shouldBe` [BulletList [[link "#a" "A"], [link "#b" "B"]]]

    it "nests a deeper heading under its parent" $
      buildTocBlocks StyleBullets [entry 1 "#a" "A", entry 2 "#b" "B"]
        `shouldBe` [BulletList [[link "#a" "A", BulletList [[link "#b" "B"]]]]]

    it "nests several levels" $
      buildTocBlocks StyleBullets [entry 1 "#a" "A", entry 2 "#b" "B", entry 3 "#c" "C"]
        `shouldBe`
          [ BulletList
              [ [ link "#a" "A"
                , BulletList [[link "#b" "B", BulletList [[link "#c" "C"]]]]
                ]
              ]
          ]

    it "returns to the parent level after a nested run" $
      buildTocBlocks StyleBullets [entry 1 "#a" "A", entry 2 "#b" "B", entry 1 "#c" "C"]
        `shouldBe`
          [ BulletList
              [ [link "#a" "A", BulletList [[link "#b" "B"]]]
              , [link "#c" "C"]
              ]
          ]

    it "treats a skipped level as one step of nesting" $
      buildTocBlocks StyleBullets [entry 1 "#a" "A", entry 3 "#c" "C"]
        `shouldBe` [BulletList [[link "#a" "A", BulletList [[link "#c" "C"]]]]]

    it "handles a document that starts deep and gets shallower" $
      buildTocBlocks StyleBullets [entry 3 "#a" "A", entry 1 "#b" "B"]
        `shouldBe` [BulletList [[link "#a" "A"], [link "#b" "B"]]]

    it "preserves formatting in labels" $
      buildTocBlocks StyleBullets [TocEntry 1 "#a" [Emph [Str "A"], Space, Code nullAttr "b"]]
        `shouldBe`
          [ BulletList
              [[Plain [Link nullAttr [Emph [Str "A"], Space, Code nullAttr "b"] ("#a", "")]]]
          ]

    it "flattens a link inside a heading so the entry is not a nested anchor" $
      buildTocBlocks StyleBullets [TocEntry 1 "#a" [Link nullAttr [Str "Docs"] ("/d", "")]]
        `shouldBe` [BulletList [[Plain [Link nullAttr [Str "Docs"] ("#a", "")]]]]

    it "drops footnotes from labels" $
      buildTocBlocks StyleBullets [TocEntry 1 "#a" [Str "A", Note [Para [Str "n"]]]]
        `shouldBe` [BulletList [[Plain [Link nullAttr [Str "A"] ("#a", "")]]]]

    it "renders a single entry" $
      buildTocBlocks StyleBullets [entry 2 "#only" "Only"]
        `shouldBe` [BulletList [[link "#only" "Only"]]]

  describe "numbered" $ do
    it "renders nothing for no entries" $
      buildTocBlocks StyleNumbered [] `shouldBe` []

    it "renders a flat list of siblings as an ordered list" $
      buildTocBlocks StyleNumbered [entry 2 "#a" "A", entry 2 "#b" "B"]
        `shouldBe` [OrderedList numAttrs [[link "#a" "A"], [link "#b" "B"]]]

    it "nests a deeper heading under its parent" $
      buildTocBlocks StyleNumbered [entry 1 "#a" "A", entry 2 "#b" "B"]
        `shouldBe`
          [OrderedList numAttrs [[link "#a" "A", OrderedList numAttrs [[link "#b" "B"]]]]]

    it "nests three levels, each starting at 1" $
      buildTocBlocks StyleNumbered [entry 1 "#a" "A", entry 2 "#b" "B", entry 3 "#c" "C"]
        `shouldBe`
          [ OrderedList numAttrs
              [ [ link "#a" "A"
                , OrderedList numAttrs [[link "#b" "B", OrderedList numAttrs [[link "#c" "C"]]]]
                ]
              ]
          ]

    it "returns to the parent level after a nested run" $
      buildTocBlocks StyleNumbered [entry 1 "#a" "A", entry 2 "#b" "B", entry 1 "#c" "C"]
        `shouldBe`
          [ OrderedList numAttrs
              [ [link "#a" "A", OrderedList numAttrs [[link "#b" "B"]]]
              , [link "#c" "C"]
              ]
          ]

    it "treats a skipped level as one step of nesting" $
      buildTocBlocks StyleNumbered [entry 1 "#a" "A", entry 3 "#c" "C"]
        `shouldBe`
          [OrderedList numAttrs [[link "#a" "A", OrderedList numAttrs [[link "#c" "C"]]]]]
