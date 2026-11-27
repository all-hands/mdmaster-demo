module Mdmaster.Core.AstSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.List (mapAccumL)
import Mdmaster.Core.Ast
import Test.Hspec
import Text.Pandoc.Definition

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

hdr :: Int -> Text -> Text -> Block
hdr lvl ident title = Header lvl (ident, [], []) [Str title]

spec :: Spec
spec = do
  describe "githubSlug" $ do
    it "lowercases and hyphenates" $
      githubSlug "Getting Started" `shouldBe` "getting-started"
    it "strips punctuation" $
      githubSlug "Hello, World!" `shouldBe` "hello-world"
    it "keeps one hyphen per space, so dropped punctuation leaves doubles" $
      githubSlug "C++ & C#" `shouldBe` "c--c"
    it "keeps existing hyphens and underscores" $
      githubSlug "well-known_thing" `shouldBe` "well-known_thing"
    it "keeps digits" $
      githubSlug "Step 2" `shouldBe` "step-2"
    it "normalises tabs to hyphens" $
      githubSlug "a\tb" `shouldBe` "a-b"
    it "keeps non-Latin letters" $
      githubSlug "Καλημέρα Κόσμε" `shouldBe` "καλημέρα-κόσμε"
    it "yields an empty slug for a heading with no slug-able characters" $
      githubSlug "!!!" `shouldBe` ""

  describe "nextSlug" $ do
    let slugs = snd . mapAccumL nextSlug M.empty
    it "returns a first-seen slug unchanged" $
      slugs ["a"] `shouldBe` ["a"]
    it "suffixes repeats with -1, -2, never reusing a number" $
      slugs ["a", "a", "a"] `shouldBe` ["a", "a-1", "a-2"]
    it "tracks different slugs independently" $
      slugs ["a", "b", "a", "b"] `shouldBe` ["a", "b", "a-1", "b-1"]
    it "skips a suffixed slug a heading already holds (github-slugger)" $ do
      slugs ["foo", "foo", "foo-1"] `shouldBe` ["foo", "foo-1", "foo-1-1"]
      slugs ["foo-1", "foo", "foo"] `shouldBe` ["foo-1", "foo", "foo-2"]
    it "never issues the same slug twice" $
      let out = slugs ["x", "x-1", "x", "x", "x-1", "x-2"]
       in length out `shouldBe` length (M.keys (M.fromList [(o, ()) | o <- out]))

  describe "documentHeadings" $ do
    it "returns headings in document order" $
      map hLevel (documentHeadings (doc [hdr 1 "a" "A", Para [Str "x"], hdr 3 "b" "B"]))
        `shouldBe` [1, 3]


    it "captures the heading inlines" $
      map hInlines (documentHeadings (doc [hdr 2 "t" "Title"]))
        `shouldBe` [[Str "Title"]]

    it "finds headings nested inside a Div" $
      map hLevel (documentHeadings (doc [Div nullAttr [hdr 2 "a" "A"]]))
        `shouldBe` [2]

    it "finds headings nested inside a block quote, in document order" $
      map headingText (documentHeadings
                         (doc [hdr 1 "top" "Top", BlockQuote [hdr 2 "inner" "Inner"], hdr 2 "tail" "Tail"]))
        `shouldBe` ["Top", "Inner", "Tail"]

    it "returns nothing for a document with no headings" $
      documentHeadings (doc [Para [Str "hello"]]) `shouldBe` []

  describe "headingText" $ do
    it "concatenates plain text" $
      headingText (Heading 1 [Str "Hello", Space, Str "World"]) `shouldBe` "Hello World"
    it "sees through emphasis" $
      headingText (Heading 1 [Emph [Str "Hi"]]) `shouldBe` "Hi"
    it "includes inline code" $
      headingText (Heading 1 [Code nullAttr "map"]) `shouldBe` "map"

  describe "linkSafeInlines" $ do
    it "unwraps a link to its label" $
      linkSafeInlines [Str "see ", Link nullAttr [Str "docs"] ("/d", "")]
        `shouldBe` [Str "see ", Str "docs"]
    it "unwraps links nested inside emphasis" $
      linkSafeInlines [Emph [Link nullAttr [Str "x"] ("/x", "")]]
        `shouldBe` [Emph [Str "x"]]
    it "drops footnotes" $
      linkSafeInlines [Str "a", Note [Para [Str "n"]]] `shouldBe` [Str "a"]
    it "leaves ordinary inlines untouched" $
      linkSafeInlines [Str "a", Space, Code nullAttr "b"]
        `shouldBe` [Str "a", Space, Code nullAttr "b"]
