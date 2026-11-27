module Mdmaster.Plugins.TocSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Marker (markerEnd, markerStart, rawHtmlBlock)
import Mdmaster.Core.Operation (OpError (..))
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.Toc (markerName, runToc, tocWalk)
import Mdmaster.Plugins.Toc.Anchor (anchorBlock)
import Mdmaster.Plugins.Toc.Gen (DocAndOptions (..))
import Mdmaster.Plugins.Toc.Options
  (AnchorMode (..), SkipTitle (..), TocMode (..), TocOptions (..), TocStyle (..)
  , tocSchema)
import Mdmaster.Plugins.Toc.Render (TocEntry (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Pandoc.Definition

opts :: [(Text, Text)] -> OptionValues
opts kvs = case validateOptions tocSchema (M.fromList kvs) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

hdr :: Int -> Text -> Block
hdr lvl title = Header lvl ("", [], []) [Str title]

hdrWithId :: Int -> Text -> Text -> Block
hdrWithId lvl ident title = Header lvl (ident, [], []) [Str title]

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

link :: Text -> Text -> Block
link target label = Plain [Link nullAttr [Str label] (target, "")]

numAttrs :: ListAttributes
numAttrs = (1, Decimal, Period)

start, end :: Block
start = rawHtmlBlock (markerStart markerName)
end   = rawHtmlBlock (markerEnd markerName)

spec :: Spec
spec = do
  describe "insertion position" $ do
    it "inserts after a leading H1" $
      runToc (opts [("skipTitle", "never")]) (doc [hdr 1 "Title", hdr 2 "A"])
        `shouldBe` Right (doc
          [ hdr 1 "Title"
          , start
          , BulletList [[link "#title" "Title", BulletList [[link "#a" "A"]]]]
          , end
          , hdr 2 "A"
          ])

    it "inserts at the very top when the document does not start with an H1" $
      runToc (opts []) (doc [Para [Str "intro"], hdr 2 "A"])
        `shouldBe` Right (doc
          [ start
          , BulletList [[link "#a" "A"]]
          , end
          , Para [Str "intro"]
          , hdr 2 "A"
          ])

    it "keeps frontmatter, which lives in Meta and never in the block list" $
      let meta = Meta (M.fromList [("title", MetaString "T")])
       in runToc (opts []) (Pandoc meta [hdr 2 "A"])
            `shouldBe` Right (Pandoc meta [start, BulletList [[link "#a" "A"]], end, hdr 2 "A"])

    it "produces an empty marker block for a document with no headings" $
      runToc (opts []) (doc [Para [Str "text"]])
        `shouldBe` Right (doc [start, end, Para [Str "text"]])

  describe "existing marker blocks" $ do
    it "replaces the contents in place, wherever the block sits" $
      runToc (opts [])
        (doc [hdr 1 "Title", Para [Str "x"], start, Para [Str "stale"], end, hdr 2 "A"])
        `shouldBe` Right (doc
          [ hdr 1 "Title"
          , Para [Str "x"]
          , start
          , BulletList [[link "#a" "A"]]
          , end
          , hdr 2 "A"
          ])

    it "does not insert a second block" $
      let out = runToc (opts []) (doc [start, end, hdr 2 "A"])
       in fmap (\(Pandoc _ bs) -> length (filter (== start) bs)) out `shouldBe` Right 1

    it "reports an unterminated marker as an operation error" $
      runToc (opts []) (doc [start, hdr 2 "A"])
        `shouldBe` Left (OpError
          ("found '" <> markerStart markerName
           <> "' with no matching '" <> markerEnd markerName <> "'"))

  describe "depth" $ do
    it "excludes headings deeper than the depth" $
      runToc (opts [("depth", "2"), ("skipTitle", "never")])
             (doc [hdr 2 "A", hdr 3 "B"])
        `shouldBe` Right (doc [start, BulletList [[link "#a" "A"]], end, hdr 2 "A", hdr 3 "B"])

    it "includes deeper headings at a higher depth" $
      runToc (opts [("depth", "3"), ("skipTitle", "never")])
             (doc [hdr 2 "A", hdr 3 "B"])
        `shouldBe` Right (doc
          [ start
          , BulletList [[link "#a" "A", BulletList [[link "#b" "B"]]]]
          , end
          , hdr 2 "A"
          , hdr 3 "B"
          ])

    it "applies the depth filter to the title too" $
      runToc (opts [("depth", "1"), ("skipTitle", "never")]) (doc [hdr 2 "A", hdr 1 "B"])
        `shouldBe` Right (doc [start, BulletList [[link "#b" "B"]], end, hdr 2 "A", hdr 1 "B"])

  describe "skipTitle" $ do
    it "auto omits a lone leading H1" $
      runToc (opts []) (doc [hdr 1 "Title", hdr 2 "A"])
        `shouldBe` Right (doc [hdr 1 "Title", start, BulletList [[link "#a" "A"]], end, hdr 2 "A"])

    it "auto keeps the title when a second H1 exists" $
      runToc (opts []) (doc [hdr 1 "Title", hdr 1 "Appendix"])
        `shouldBe` Right (doc
          [ hdr 1 "Title"
          , start
          , BulletList [[link "#title" "Title"], [link "#appendix" "Appendix"]]
          , end
          , hdr 1 "Appendix"
          ])

    it "always omits the leading H1 even when a second H1 exists" $
      runToc (opts [("skipTitle", "always")]) (doc [hdr 1 "Title", hdr 1 "Appendix"])
        `shouldBe` Right (doc
          [ hdr 1 "Title", start, BulletList [[link "#appendix" "Appendix"]], end, hdr 1 "Appendix" ])

    it "keeps the first heading in every mode when it is not an H1" $
      let d = doc [hdr 2 "First", hdr 3 "Later"]
       in ( runToc (opts [("skipTitle", "always")]) d
          , runToc (opts []) d )
            `shouldBe`
              ( Right (doc [ start
                           , BulletList [[link "#first" "First", BulletList [[link "#later" "Later"]]]]
                           , end
                           , hdr 2 "First"
                           , hdr 3 "Later" ])
              , Right (doc [ start
                           , BulletList [[link "#first" "First", BulletList [[link "#later" "Later"]]]]
                           , end
                           , hdr 2 "First"
                           , hdr 3 "Later" ])
              )

  describe "duplicate headings" $
    it "numbers duplicate slugs across the whole document" $
      runToc (opts [("skipTitle", "never")]) (doc [hdr 2 "Notes", hdr 2 "Notes"])
        `shouldBe` Right (doc
          [ start
          , BulletList [[link "#notes" "Notes"], [link "#notes-1" "Notes"]]
          , end
          , hdr 2 "Notes"
          , hdr 2 "Notes"
          ])

  describe "nested headings" $
    it "keeps title-skip and slug numbering correct when the first heading sits inside a Div" $
      runToc (opts []) (doc [Div nullAttr [hdr 1 "Title"], hdr 2 "Notes", hdr 2 "Notes"])
        `shouldBe` Right (doc
          [ start
          , BulletList [[link "#notes" "Notes"], [link "#notes-1" "Notes"]]
          , end
          , Div nullAttr [hdr 1 "Title"]
          , hdr 2 "Notes"
          , hdr 2 "Notes"
          ])

  describe "anchors: pandoc mode" $
    it "links to the identifier already on the heading" $
      runToc (opts [("anchors", "pandoc"), ("skipTitle", "never")])
             (doc [hdrWithId 2 "sec-one" "Section One"])
        `shouldBe` Right (doc
          [start, BulletList [[link "#sec-one" "Section One"]], end, hdrWithId 2 "sec-one" "Section One"])

  describe "anchors: explicit mode" $ do
    it "injects an anchor before each included heading" $
      runToc (opts [("anchors", "explicit"), ("skipTitle", "never")]) (doc [hdr 2 "A"])
        `shouldBe` Right (doc
          [start, BulletList [[link "#a" "A"]], end, anchorBlock "a", hdr 2 "A"])

    it "does not anchor headings excluded by depth" $
      runToc (opts [("anchors", "explicit"), ("depth", "2"), ("skipTitle", "never")])
             (doc [hdr 2 "A", hdr 3 "B"])
        `shouldBe` Right (doc
          [start, BulletList [[link "#a" "A"]], end, anchorBlock "a", hdr 2 "A", hdr 3 "B"])

    it "places the TOC after the anchor and title when the document starts with an H1" $
      runToc (opts [("anchors", "explicit"), ("skipTitle", "never")])
             (doc [hdr 1 "Title", hdr 2 "A"])
        `shouldBe` Right (doc
          [ anchorBlock "title"
          , hdr 1 "Title"
          , start
          , BulletList [[link "#title" "Title", BulletList [[link "#a" "A"]]]]
          , end
          , anchorBlock "a"
          , hdr 2 "A"
          ])

    it "replaces rather than accumulates our anchors on a re-run" $
      runToc (opts [("anchors", "explicit"), ("skipTitle", "never")])
             (doc [anchorBlock "stale", hdr 2 "A"])
        `shouldBe` Right (doc
          [start, BulletList [[link "#a" "A"]], end, anchorBlock "a", hdr 2 "A"])

    it "never touches user-authored anchors" $
      let userAnchor = rawHtmlBlock "<a id=\"mine\"></a>"
       in runToc (opts [("anchors", "explicit"), ("skipTitle", "never")])
                 (doc [userAnchor, hdr 2 "A"])
            `shouldBe` Right (doc
              [start, BulletList [[link "#a" "A"]], end, userAnchor, anchorBlock "a", hdr 2 "A"])

  describe "switching away from explicit mode" $
    it "removes anchors left behind by a previous explicit run" $
      runToc (opts [("anchors", "github"), ("skipTitle", "never")])
             (doc [anchorBlock "a", hdr 2 "A"])
        `shouldBe` Right (doc [start, BulletList [[link "#a" "A"]], end, hdr 2 "A"])

  describe "style: numbered" $ do
    it "renders the entries as nested ordered lists, each level starting at 1" $
      runToc (opts [("style", "numbered"), ("skipTitle", "never")])
             (doc [hdr 2 "A", hdr 3 "B"])
        `shouldBe` Right (doc
          [ start
          , OrderedList numAttrs [[link "#a" "A", OrderedList numAttrs [[link "#b" "B"]]]]
          , end
          , hdr 2 "A"
          , hdr 3 "B"
          ])

    it "spelled bullets behaves the same as the default" $
      let d = doc [hdr 2 "A", hdr 3 "B"]
       in runToc (opts [("style", "bullets"), ("skipTitle", "never")]) d
            `shouldBe` runToc (opts [("skipTitle", "never")]) d

    it "is idempotent under a re-run" $
      let o = opts [("style", "numbered"), ("skipTitle", "never")]
          d = doc [hdr 2 "A", hdr 3 "B", hdr 2 "C"]
       in (runToc o d >>= runToc o) `shouldBe` runToc o d

  describe "mode: update" $
    it "behaves the same whether the option is spelled or absent" $ do
      let d = doc [hdr 1 "Title", start, Para [Str "stale"], end, hdr 2 "A"]
      runToc (opts [("mode", "update")]) d `shouldBe` runToc (opts []) d

  describe "mode: remove" $ do
    it "deletes the marker block whole, wherever it sits" $
      runToc (opts [("mode", "remove")])
        (doc [hdr 1 "Title", start, BulletList [[link "#a" "A"]], end, hdr 2 "A"])
        `shouldBe` Right (doc [hdr 1 "Title", hdr 2 "A"])

    it "strips the anchors explicit mode injected alongside the block" $
      runToc (opts [("mode", "remove")])
        (doc [anchorBlock "title", hdr 1 "Title", start, Para [Str "stale"], end,
              anchorBlock "a", hdr 2 "A"])
        `shouldBe` Right (doc [hdr 1 "Title", hdr 2 "A"])

    it "never touches user-authored anchors" $
      let userAnchor = rawHtmlBlock "<a id=\"mine\"></a>"
       in runToc (opts [("mode", "remove")]) (doc [userAnchor, hdr 2 "A"])
            `shouldBe` Right (doc [userAnchor, hdr 2 "A"])

    it "leaves a document with neither block nor anchors unchanged" $
      let d = doc [hdr 1 "Title", Para [Str "body"], hdr 2 "A"]
       in runToc (opts [("mode", "remove")]) d `shouldBe` Right d

    it "reports an unterminated marker as an operation error too" $
      runToc (opts [("mode", "remove")]) (doc [start, hdr 2 "A"])
        `shouldBe` Left (OpError
          ("found '" <> markerStart markerName
           <> "' with no matching '" <> markerEnd markerName <> "'"))

    it "gives depth, skipTitle, anchors and style no say" $
      runToc (opts [("mode", "remove"), ("depth", "1"), ("skipTitle", "always"),
                    ("anchors", "explicit"), ("style", "numbered")])
             (doc [hdr 1 "Title", hdr 2 "A", start, Para [Str "stale"], end])
        `shouldBe` Right (doc [hdr 1 "Title", hdr 2 "A"])

  describe "tocWalk" $ do
    it "leaves github-mode documents unchanged and returns the entries in document order" $
      let d = doc [hdr 1 "Title", hdr 3 "Deep", hdr 3 "Deep"]
       in tocWalk (TocOptions TocUpdate 6 SkipNever AnchorGithub StyleBullets) d
            `shouldBe` ( d
                       , [ TocEntry 1 "#title" [Str "Title"]
                         , TocEntry 3 "#deep" [Str "Deep"]
                         , TocEntry 3 "#deep-1" [Str "Deep"] ])

    it "omits depth-excluded and skipped headings from the entries" $
      let d = doc [hdr 1 "Title", hdr 4 "Deep"]
       in tocWalk (TocOptions TocUpdate 2 SkipAlways AnchorGithub StyleBullets) d
            `shouldBe` (d, [])

  describe "idempotency" $ do
    prop "running twice equals running once" $ \(DocAndOptions d o) ->
      (runToc o d >>= runToc o) == runToc o d

    prop "a successful run never leaves an unterminated marker" $ \(DocAndOptions d o) ->
      case runToc o d of
        Left _     -> True
        Right out  -> case runToc o out of
                        Left _  -> False
                        Right _ -> True

  describe "explicit anchors do not accumulate" $
    prop "a third run equals the second" $ \(DocAndOptions d o) ->
      (runToc o d >>= runToc o >>= runToc o) == (runToc o d >>= runToc o)
