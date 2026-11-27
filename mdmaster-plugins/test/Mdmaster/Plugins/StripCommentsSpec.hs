module Mdmaster.Plugins.StripCommentsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Marker (markerEnd, markerStart, rawHtmlBlock)
import Mdmaster.Core.Operation (OpError (..))
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.Docx.Collect (Comment (..), Thread (..))
import Mdmaster.Plugins.Docx.Options (CommentStyle (..))
import Mdmaster.Plugins.Docx.Render (Placement (..), renderComments)
import Mdmaster.Plugins.StripComments (runStripComments)
import Mdmaster.Plugins.StripComments.Options (stripCommentsSchema)
import Test.Hspec
import Text.Pandoc.Definition

opts :: [(Text, Text)] -> OptionValues
opts kvs = case validateOptions stripCommentsSchema (M.fromList kvs) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

run :: [(Text, Text)] -> Pandoc -> Either OpError Pandoc
run kvs = runStripComments (opts kvs)

para :: Text -> Block
para t = Para [Str t]

comment :: Text -> Block
comment = rawHtmlBlock

inlineComment :: Text -> Inline
inlineComment = RawInline (Format "html")

archived :: Thread
archived = Thread
  { tBlockIndex = 0
  , tAnchor     = [Str "anchor"]
  , tRoot       = Comment
      { cId = "0", cDurableId = "", cAuthor = "A", cDate = "2024-01-01T09:00:00Z"
      , cDateLocal = "2024-01-01T09:00:00Z", cInitials = "A", cDone = False
      , cParent = Nothing, cBody = [Str "note"] }
  , tReplies    = []
  }

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

dirty :: Pandoc
dirty = doc
  [ para "before"
  , comment "<!-- ordinary -->"
  , rawHtmlBlock (markerStart "toc")
  , para "content"
  , rawHtmlBlock (markerEnd "toc")
  , Para [Str "text", Space, inlineComment "<!-- c -->", Space, Str "more"]
  , para "after"
  ]

spec :: Spec
spec = do
  describe "block comments" $ do
    it "removes a comment on its own line, leaving the prose either side" $
      run [] (doc [para "before", comment "<!-- a note -->", para "after"])
        `shouldBe` Right (doc [para "before", para "after"])

    it "removes every comment, not just the first" $
      run [] (doc [comment "<!-- one -->", para "x", comment "<!-- two -->"])
        `shouldBe` Right (doc [para "x"])

    it "removes a multi-line comment" $
      run [] (doc [para "x", comment "<!-- first\n   second -->", para "y"])
        `shouldBe` Right (doc [para "x", para "y"])

    it "removes two comments the reader gathered into one raw block" $
      run [] (doc [comment "<!-- a --> <!-- b -->", para "x"])
        `shouldBe` Right (doc [para "x"])

    it "removes a comment nested in a container" $
      run [] (doc [BlockQuote [para "kept", comment "<!-- gone -->"]])
        `shouldBe` Right (doc [BlockQuote [para "kept"]])

  describe "comments inside a paragraph" $ do
    it "removes the raw inline and leaves the surrounding inlines" $
      run [] (doc [Para [Str "a", Space, inlineComment "<!-- c -->", Space, Str "b"]])
        `shouldBe` Right (doc [Para [Str "a", Space, Space, Str "b"]])

    it "removes one at the end of a heading too" $
      run [] (doc [Header 1 nullAttr [Str "Title", Space, inlineComment "<!-- c -->"]])
        `shouldBe` Right (doc [Header 1 nullAttr [Str "Title", Space]])

  describe "what is not a comment" $ do
    it "leaves raw HTML that is not a comment alone" $
      run [] (doc [para "x", RawBlock (Format "html") "<br>", para "y"])
        `shouldBe` Right (doc [para "x", RawBlock (Format "html") "<br>", para "y"])

    it "leaves a node mixing a comment with markup alone rather than editing inside it" $
      run [] (doc [RawBlock (Format "html") "<!-- note --><br>", para "x"])
        `shouldBe` Right (doc [RawBlock (Format "html") "<!-- note --><br>", para "x"])

  describe "marker comments" $ do
    let marked = doc
          [ para "before"
          , rawHtmlBlock (markerStart "toc")
          , para "content"
          , rawHtmlBlock (markerEnd "toc")
          , para "after"
          ]

    it "keeps comments carrying a marker line, by default" $
      run [] marked `shouldBe` Right marked

    it "strips marker comments like any other when keepMarkers is false" $
      run [("keepMarkers", "false")] marked
        `shouldBe` Right (doc [para "before", para "content", para "after"])

    it "keeps markers an earlier release wrote, before the rename to mdmaster" $
      let legacy = doc [comment "<!-- mdaster:toc start -->", para "c", comment "<!-- mdaster:toc end -->"]
       in run [] legacy `shouldBe` Right legacy

    it "keeps a node holding a marker and an ordinary comment together" $
      run [] (doc [comment (markerStart "toc" <> "\n<!-- plain -->"), para "x"])
        `shouldBe` Right (doc [comment (markerStart "toc" <> "\n<!-- plain -->"), para "x"])

    it "keeps a docx archive record by default: its opening is the marker prefix" $
      let record = case renderComments Archive [archived] of
            Inline [(_, bs@(_ : _))] -> bs
            other -> error ("expected one archive record, got " <> show other)
       in run [] (doc (para "x" : record)) `shouldBe` Right (doc (para "x" : record))

    it "does not spare a comment that mentions mdmaster without the marker spelling" $
      run [] (doc [comment "<!-- mdmaster was here -->", para "x"])
        `shouldBe` Right (doc [para "x"])

  describe "a document with nothing to strip" $
    it "is returned untouched" $
      let d = doc [Header 1 nullAttr [Str "Title"], para "body"]
       in run [] d `shouldBe` Right d

  describe "idempotency" $ do
    it "a second run changes nothing, with markers kept" $
      (run [] dirty >>= run []) `shouldBe` run [] dirty

    it "a second run changes nothing, with markers stripped" $
      let off = run [("keepMarkers", "false")]
       in (off dirty >>= off) `shouldBe` off dirty
