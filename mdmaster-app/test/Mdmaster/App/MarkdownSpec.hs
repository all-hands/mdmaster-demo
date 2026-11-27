module Mdmaster.App.MarkdownSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Mdmaster.App.Markdown (parseMarkdown, renderMarkdown)
import Test.Hspec
import Text.Pandoc.Definition

roundTrip :: T.Text -> Either T.Text T.Text
roundTrip t = parseMarkdown t >>= renderMarkdown

item :: T.Text -> [Block]
item t = [Plain [Str t]]

spec :: Spec
spec = do
  describe "parseMarkdown" $ do
    it "parses a heading" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "# Title\n")
        `shouldBe` Right [Header 1 ("title", [], []) [Str "Title"]]

    it "assigns gfm identifiers to headings" $
      fmap (\(Pandoc _ bs) -> [i | Header _ (i, _, _) _ <- bs])
           (parseMarkdown "## Getting Started\n")
        `shouldBe` Right ["getting-started"]

    it "puts YAML frontmatter into the metadata, not the blocks" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "---\ntitle: T\n---\n\n# H\n")
        `shouldBe` Right [Header 1 ("h", [], []) [Str "H"]]

    it "keeps an HTML comment as its own raw block" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "<!-- mdmaster:toc start -->\n\ntext\n")
        `shouldBe` Right
          [ RawBlock (Format "html") "<!-- mdmaster:toc start -->"
          , Para [Str "text"]
          ]

    it "normalises CRLF line endings" $
      parseMarkdown "# A\r\n\r\ntext\r\n" `shouldBe` parseMarkdown "# A\n\ntext\n"

  describe "parseMarkdown and typography" $ do
    it "leaves quotes, apostrophes and dash runs as typed" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "\"hi\" don't -- wait...\n")
        `shouldBe` Right [Para [ Str "\"hi\"", Space, Str "don't", Space
                               , Str "--", Space, Str "wait..." ]]

    it "keeps the space after an abbreviation an ordinary space" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "Mr. Smith\n")
        `shouldBe` Right [Para [Str "Mr.", Space, Str "Smith"]]

    it "gives an autolink its URL as its one Str of text" $
      fmap (\(Pandoc _ bs) -> bs) (parseMarkdown "<https://example.com/'x'>\n")
        `shouldBe` Right [Para [Link ("", ["uri"], []) [Str "https://example.com/'x'"]
                                     ("https://example.com/'x'", "")]]

  describe "renderMarkdown" $ do
    it "ends output with exactly one newline" $
      roundTrip "# Title\n" `shouldBe` Right "# Title\n"

    it "does not wrap long paragraphs" $
      let long = T.replicate 30 "word " <> "end"
       in fmap (length . T.lines) (roundTrip (long <> "\n")) `shouldBe` Right 1

    it "round-trips YAML frontmatter" $
      roundTrip "---\ntitle: T\n---\n\n# H\n"
        `shouldBe` Right "---\ntitle: T\n---\n\n# H\n"

    it "round-trips a fenced code block verbatim" $
      roundTrip "``` haskell\nmain = pure ()\n```\n"
        `shouldBe` Right "``` haskell\nmain = pure ()\n```\n"

    it "separates a raw HTML block from the heading that follows it" $
      roundTrip "<a id=\"x\" data-mdmaster></a>\n\n## H\n"
        `shouldBe` Right "<a id=\"x\" data-mdmaster></a>\n\n## H\n"

    it "renders a link with a fragment target" $
      renderMarkdown (Pandoc nullMeta [Plain [Link nullAttr [Str "A"] ("#a", "")]])
        `shouldBe` Right "[A](#a)\n"

    it "writes curly quotes, dashes and ellipses literally" $
      renderMarkdown (Pandoc nullMeta [Para [Str "\8220hi\8221\8230 don\8217t \8211 wait"]])
        `shouldBe` Right "\8220hi\8221\8230 don\8217t \8211 wait\n"

    it "round-trips curly typography unchanged, so a second smart run has nothing to do" $
      roundTrip "he said \8220hi\8221\8230 don\8217t \8212 wait\n"
        `shouldBe` Right "he said \8220hi\8221\8230 don\8217t \8212 wait\n"

  describe "ordered list start numbers" $ do
    it "renders a list starting at 4 as 4., not 1." $
      renderMarkdown (Pandoc nullMeta [OrderedList (4, Decimal, Period) [item "a", item "b"]])
        `shouldBe` Right "4.  a\n5.  b\n"

    it "keeps a list interrupted by a paragraph counting through it" $
      renderMarkdown
        (Pandoc nullMeta
          [ OrderedList (1, Decimal, Period) [item "one"]
          , Para [Str "interruption"]
          , OrderedList (2, Decimal, Period) [item "two", item "three"]
          ])
        `shouldBe` Right "1.  one\n\ninterruption\n\n2.  two\n3.  three\n"

    it "reads a start value rather than renumbering it away" $
      fmap (\(Pandoc _ bs) -> [n | OrderedList (n, _, _) _ <- bs])
           (parseMarkdown "4. a\n5. b\n")
        `shouldBe` Right [4]

    it "round-trips a list that starts at 4" $
      roundTrip "4. a\n5. b\n" `shouldBe` Right "4.  a\n5.  b\n"

  describe "failure" $
    it "reports a reader failure as Left rather than throwing" $
      isLeft (parseMarkdown "---\ntitle: [unclosed\n---\n") `shouldBe` True
